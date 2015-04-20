package edu.berkeley.cs.boom.bloomscala.analysis

import edu.berkeley.cs.boom.bloomscala.ast._
import edu.berkeley.cs.boom.bloomscala.typing.CollectionType
import org.kiama.attribution.Attribution._
import org.kiama.rewriting.PositionalRewriter._
import org.kiama.util.{Positioned, Messaging}
import org.kiama.attribution.{Attribution, Attributable}
import edu.berkeley.cs.boom.bloomscala.stdlib.{UnknownFunction, BuiltInFunctions}

/**
 * Rewrites the AST to bind field, function, and collection references.
   PAA: hoist up modules too!
 */
class Namer(messaging: Messaging) {

  import messaging.message
  import sext._

  def resolveNames(program: Program): Program = {
    val interm_prog1 = rewrite(everywhere(bindModuleRef))(program)
    Attribution.initTree(interm_prog1)
    val interm_prog = rewrite(everywhere(bindImportRef))(interm_prog1)
    //println(s"INTERM: ${interm_prog.treeString}")
    val bindings = everywhere(bindCollectionRef) <* everywhere(bindTableRef) <* everywhere(bindFieldRef) <* everywhere(bindFunctionRef)
    // why, oh why?
    Attribution.initTree(interm_prog)
    rewrite(bindings)(interm_prog)

  }

  def containsPrograms(program: Program): Int = {
    program.nodes.map {
      case p: Program => containsPrograms(p) + 1
      case _ => 0
    }.sum
  }

  private val bindModuleRef =
    rule {
      case n: Include => bindModule(n)
    }

  private val bindImportRef =
    rule {
      case m: Import => bindImport(m)
    }

  private val bindCollectionRef =
    rule {
      case fc: FreeCollectionRef =>
        bind(fc)
      case ftv: FreeTupleVariable =>
        bind(ftv)
    }

  private val bindFieldRef =
    rule {
      case fr: FreeFieldRef =>
        bindField(fr)
    }

  private val bindFunctionRef =
    rule {
      case fr: FreeFunctionRef =>
        bindFunction(fr)
    }

  private val bindTableRef =
    rule {
      case mr: MappedCollection => bindMapCR(mr)
      case j: JoinedCollections => bindJoins(j)
    }

  private implicit def mangleCR: String => CollectionRef => CollectionRef =
    paramAttr {
      name => {
        //case FreeCollectionRef(nm) => FreeCollectionRef(s"$name_$nm")
        case FreeCollectionRef(nm) => FreeCollectionRef(List(name, nm).mkString("_"))
        case BoundCollectionRef(nm, col, lan) => BoundCollectionRef(List(name, nm).mkString("_"), col, lan)
        case m => m
      }
    }

  private implicit def bindModule: Include => Program =
    attr {
      case i @ Include(mod) =>
        val module = i->lookupModule(mod)
        Program(module.nodes)
      case m => m
    }

  private implicit def bindImport: Import => Program =
    attr {
      case i @ Import(mod, alias) =>
        // here is the problem.  this module is "raw."
        // we should't touch it until we're sure that its module nestings
        // are completely resolved.
        val modNodes = i->lookupModule(mod)
        val rl1 = rule {
          case f: CollectionRef => mangleCR(alias)(f)
        }
        val rl2 = rule {
          case CollectionDeclaration(typ, name, keys, vals) =>
            val mangled = List(alias, name).mkString ("_")
            typ match {
              case CollectionType.Output => CollectionDeclaration(CollectionType.Scratch, mangled, keys, vals)
              case CollectionType.Input => CollectionDeclaration(CollectionType.Scratch, mangled, keys, vals)
              case typ => CollectionDeclaration(typ, mangled, keys, vals)
            }
        }

        val mn = rewrite(everywhere(bindImportRef) <* everywhere(bindModuleRef) <* everywhere(rl2) <* everywhere(rl1))(modNodes)
        Program(mn.nodes)
    }

  private implicit def bindMapCR: MappedCollection => MappedCollection =
    attr {
      case MappedCollection(collection, tupleVars, e: TableRefExpr) =>
        val colList = (collection.collection.keys ++ collection.collection.values).map(c => BoundFieldRef(collection, c.name, Field(e.alias, c.typ)))
        MappedCollection(collection, tupleVars, RowExpr(colList))
      case m => m
    }

  private implicit def bindJoins: JoinedCollections => JoinedCollections =
    attr {
      case JoinedCollections(collections, preds, tupleVars, e: TableRefExpr) =>
        val thisCollection = collections(tupleVars.indexOf(e.alias))
        val colList = (thisCollection.collection.keys ++ thisCollection.collection.values).map(c => BoundFieldRef(thisCollection, c.name, Field(e.alias, c.typ)))
        JoinedCollections(collections, preds, tupleVars, RowExpr(colList))
      case j => j
    }

  private implicit def bind: CollectionRef => BoundCollectionRef =
    attr {
      case bound: BoundCollectionRef =>
        bound
      case tv @ FreeTupleVariable(name) =>
        tv->lookupTupleVar(name) match {
        case (md: MissingDeclaration, _) =>
          message(tv, s"Unknown tuple variable $name")
          BoundCollectionRef(name, md, -1)
        case (cd, lambdaArgNumber) =>
          BoundCollectionRef(name, cd, lambdaArgNumber)
      }
      case cr @ FreeCollectionRef(name) =>
        cr->lookup(name) match {
        case md: MissingDeclaration =>
          message(cr, s"Unknown collection $name")
          BoundCollectionRef(name, md, 0)
        case cd =>
          BoundCollectionRef(name, cd, 0)
      }
    }

  private implicit def bindField: FreeFieldRef => BoundFieldRef =
    attr {
      case fr @ FreeFieldRef(cr @ BoundCollectionRef(colName, decl, _), fieldName) =>
        val field = decl.getField(fieldName).getOrElse {
          message(fr, s"Collection $colName does not have field $fieldName")
          new UnknownField
        }
        new BoundFieldRef(cr, fieldName, field)
    }

  private implicit def bindFunction: FreeFunctionRef => BoundFunctionRef =
    attr {
      case fr @ FreeFunctionRef(name) =>
        val func = BuiltInFunctions.nameToFunction.getOrElse(name, {
          message(fr, s"Could not find function named $name")
          UnknownFunction
        })
        BoundFunctionRef(name, func)
    }

  private lazy val tupleVarBindingTargets: MappedCollectionTarget => Seq[CollectionRef] =
    attr {
      case JoinedCollections(collections, _, _, _) => collections.toSeq
      case cr: CollectionRef => Seq(cr)
    }

  private def checkTupleVarCount(expectedSize: Int, actual: List[String], loc: Positioned) {
    if (actual.size != expectedSize) {
      message(loc, s"Wrong number of tuple vars; expected $expectedSize but got ${actual.size}")
    }
  }

  //private lazy val lookupModule: String => Attributable => Seq[Node] =
  private lazy val lookupModule: String => Attributable => Module =
    paramAttr {
      name => {
        case program: Program =>
          program.modules.find{m => m.name == name}.getOrElse{
            if (program.parent == null) {
              message(program, s"Couldn't find module $name among ${program.modules.map(_.name)}")
              new MissingModule
            } else {
              program.parent->lookupModule(name)
            }
          }
        case n => n.parent->lookupModule(name)
      }
    }

  private lazy val lookupTupleVar: String => Attributable => (CollectionDeclaration, Int) =
    paramAttr {
      name => {
        case join @ JoinedCollections(_, _, tupleVars, _) =>
          val targets = tupleVarBindingTargets(join)
          checkTupleVarCount(targets.size, tupleVars, join)
          val lambdaArgNumber = tupleVars.indexOf(name)
          if (lambdaArgNumber == -1) {
            (new MissingDeclaration, -1)
          } else {
            (bind(targets(lambdaArgNumber)).collection, lambdaArgNumber)
          }
        case mc @ MappedCollection(target, tupleVars, _) =>
          val targets = tupleVarBindingTargets(target)
          checkTupleVarCount(targets.size, tupleVars, mc)
          if (tupleVars.head != name) {
            (new MissingDeclaration, -1)
          } else {
            (bind(targets.head).collection, 0)
          }
        case program: Program =>
          throw new IllegalStateException("A tuple variable appeared in an invalid context")
        case n =>
          n.parent->lookupTupleVar(name)
      }
    }

  private lazy val lookup: String => Attributable => CollectionDeclaration =
    paramAttr {
      name => {
        case module: Module =>
          val colls: Traversable[CollectionDeclaration] =
            module.nodes.filter(_.isInstanceOf[CollectionDeclaration]).map(_.asInstanceOf[CollectionDeclaration]) ++
            module.nodes.filter(_.isInstanceOf[Program]).map(_.asInstanceOf[Program].declarations).flatten
          val decl = colls.find(_.name == name)
          decl.getOrElse(new MissingDeclaration())

        case program: Program =>
          val decl = program.declarations.find(_.name == name)
          decl.getOrElse(new MissingDeclaration())
        case n => n.parent->lookup(name)
      }
    }
}
