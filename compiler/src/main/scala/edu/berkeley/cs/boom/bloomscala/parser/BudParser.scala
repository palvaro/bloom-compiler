package edu.berkeley.cs.boom.bloomscala.parser

import edu.berkeley.cs.boom.bloomscala.ast._
import org.kiama.util.PositionedParserUtilities
import org.kiama.attribution.Attribution
import edu.berkeley.cs.boom.bloomscala.rewriting.InitialRewrites
import edu.berkeley.cs.boom.bloomscala.typing.{UnknownType, CollectionType, FieldType}

trait   BudParser extends PositionedParserUtilities {

  /********** Helper Methods **********/

  def alternatives[T](map: Map[String, T], failMsg: String) =
    (map.keySet.map(literal).reduce(_ | _) | failure(failMsg)) ^^ map.apply

  def listOf[T](t: Parser[T]): Parser[List[T]] = "[" ~> repsep(t, ",") <~ "]"

  lazy val collectionType = alternatives(CollectionType.nameToType, "Invalid collection type")
  lazy val bloomOp = alternatives(BloomOp.symbolToOp, "Invalid operator")
  lazy val fieldType = alternatives(FieldType.nameToType, "Invalid field type")

  /********** Declarations **********/

  lazy val ident = opt(":") ~> "[a-zA-Z][a-zA-Z0-9_]*".r

  lazy val lbrac = ("do" | "{")
  lazy val rbrac = ("end" | "}")

  lazy val collectionDeclaration: Parser[CollectionDeclaration] = {
    def columnsDeclaration: Parser[List[Field]] = listOf(tableColumn)
    def tableColumn = opt("@") ~ ident ~ opt(":" ~ fieldType) ^^ {
      case Some(s) ~ i ~ t => Field(i, FieldType("location"))
      case None ~ i ~ Some(":" ~ f) => Field(i, f)
      case None ~ i ~ None => Field(i, FieldType("string"))
    }

    (collectionType ~ ident ~ "," ~ opt(columnsDeclaration ~ opt("=>" ~> columnsDeclaration))) ^^ {
      case collectionType ~  ident ~ "," ~ keyVals =>
        val keys = keyVals.map(_._1).getOrElse(List.empty)
        val values = keyVals.flatMap(_._2).getOrElse(List.empty)
        new CollectionDeclaration(collectionType, ident, keys, values)
    }
  }

  /********** Statements **********/

  lazy val number = "[0-9]+".r ^^ { s => ConstantColExpr(s, FieldType.BloomInt)}
  lazy val string = "\"[^\"]*\"".r ^^ { s => ConstantColExpr(s.stripPrefix("\"").stripSuffix("\""), FieldType.BloomString)}
  lazy val constant = string | number

  lazy val collectionRef = ident ^^ { i => FreeCollectionRef(i) }

  lazy val fieldRef = (collectionRef ~ "." ~ ident) ^^ {
    case collection ~ "." ~ field =>
      FreeFieldRef(collection, field)
  }

  lazy val functionCall = ident ~ "(" ~ repsep(colExpr, ",") ~ ")" ^^ {
    case name ~ "(" ~ args ~ ")" => FunctionCall(FreeFunctionRef(name), args)
  }
  lazy val colTerm = fieldRef
  lazy val colExpr: Parser[ColExpr] = {
    def plus = colTerm ~ "+" ~ colExpr ^^ {case a ~ "+" ~ b => PlusStatement(a, b, UnknownType())}
    plus | functionCall | colTerm | constant
  }
  //lazy val rowExpr: Parser[RowExpr] = listOf(colExpr) ^^ RowExpr
  lazy val rowExpr: Parser[Expr] = (listOf(colExpr) ^^ RowExpr)| (ident ^^ TableRefExpr)

  lazy val predicate: Parser[Predicate] = {
    lazy val eqPred = colExpr ~ "==" ~ colExpr ^^ { case a ~ "==" ~ b => EqualityPredicate(a, b)}
    eqPred
  }

  lazy val expr: Parser[Expr] = colExpr | rowExpr

  lazy val statement: Parser[Statement] = {
    lazy val lhs = collectionRef
    lazy val rhs = collectionMap | derivedCollection | collectionRef | facts

    lazy val collection = collectionRef | derivedCollection
    lazy val derivedCollection = join | notin | argmin

    lazy val facts = listOf(listOf(constant)) ^^ {case recs => Facts(recs.map(r => RowExpr(r)))}

    // i.e. (link * path) on (link.to == path.from)
    lazy val join = "(" ~ rep1sep(collectionRef, "*") ~ ")" ~ "on" ~ "(" ~ rep1sep(predicate, ",") ~ ")" ~ mapBlock ^^ {
      case "(" ~ collections ~ ")" ~ "on" ~ "(" ~ predicates ~ ")" ~ tupleVarsRowExpr =>
        JoinedCollections(collections, predicates, tupleVarsRowExpr._1, tupleVarsRowExpr._2)
    }

    lazy val notin = collectionRef ~ "." ~ "notin" ~ "(" ~ collectionRef ~")" ^^ {
      case a ~ "." ~ "notin" ~ "(" ~ b ~ ")" => new NotIn(a, b)
    }

    lazy val mapBlock = ("{" ~> "|" ~> rep1sep(ident, ",") <~ "|") ~ rowExpr <~ "}" ^^ {
      case tupleVars ~ rowExpr => (tupleVars, rowExpr)
    }

    lazy val collectionMap = collectionRef ~ mapBlock ^^ {
      case collection ~ tupleVarsRowExpr =>
        MappedCollection(collection, tupleVarsRowExpr._1, tupleVarsRowExpr._2)
    }

    lazy val argmin = collectionRef ~ "." ~ "argmin" ~ "(" ~ listOf(fieldRef) ~ "," ~ expr ~ "," ~ ident ~ ")" ^^ {
      case collection ~ "." ~ "argmin" ~ "(" ~ groupingCols ~ "," ~ chooseExpr ~ "," ~ func ~ ")" =>
        ArgMin(collection, groupingCols, chooseExpr, FreeFunctionRef(func))
    }

    lhs ~ bloomOp ~ rhs ^^ { case l ~ o ~ r => Statement(l, o, r)}
  }

  lazy val blockDeclaration: Parser[List[Node]] = {
    ("bloom" | "state") ~> opt(ident) ~> lbrac ~> rep(statement | collectionDeclaration ) <~ rbrac
  }

  lazy val moduleDeclaration: Parser[List[Node]] = {
  //lazy val moduleDeclaration = {
    ("module" | "class") ~ ident ~ lbrac ~ rep(blockDeclaration | includeCalls) ~ rbrac ^^ {
    //("module" | "class") ~ ident ~ lbrac ~ rep(blockDeclaration) ~ rbrac ^^ {
      // grrrr
      //case (name ~ xs) => List(Module(xs.flatten, name))
      case ("module" | "class") ~ name ~ lbrac ~ xs ~ rbrac => List(Module(xs.flatten, name))
    }
  }

  lazy val importCall = {
    "import" ~ ident ~ "=>" ~ ident ^^ {
      case "import" ~ mod ~ "=>" ~ name => Import(mod, name)
    }
  }

  lazy val includeCalls: Parser[List[Node]] = {
    "include" ~ ident  ^^ {
      case "include" ~ mod  => List(Include(mod))
    }
  }

  // hack attack!
  lazy val includeCall: Parser[Node] = {
    "include" ~ ident  ^^ {
      case "include" ~ mod  => Include(mod)
    }
  }

  override val whiteSpace =
    """(\s|(//.*\n))+""".r


  lazy val program: Parser[Program] = {
    lazy val topLevelDef = (includeCall | statement | collectionDeclaration) ^^ { case x => List(x) }
    rep(blockDeclaration | moduleDeclaration | topLevelDef) ^^ {
    //rep(moduleDeclaration| blockDeclaration |  topLevelDef) ^^ {
      case listOfListsOfNodes => {
        Program(listOfListsOfNodes.flatten)
      }
    }
  }
}

object BudParser extends BudParser {
  def parseProgram(str: CharSequence): Program = {
    val p = parseAll(program, str).get
    // When I tried to apply these rewrites after initializing the tree, it appears
    // that the new tree could contain references to the root of the old tree via
    // `parent` pointers, which could cause certain traversal paths to not see
    // the effects of the rewrite.
    // TODO: is this a bug in Kiama or misuse on my part?
    val rewritten = InitialRewrites(p)
    Attribution.initTree(rewritten)
    rewritten
  }
}