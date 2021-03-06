function Bloom () {
    var rx = require('rx');
    var rxflow = require('rxflow');
    var currentTick = 0;

    var tables = {
        "path": new rxflow.Table(3) /* table path, [from: string, to: string, nxt: string, cost: int] */
    };

    var inputs = {
        "link": new rx.Subject() /* input link, [from: string, to: string, cost: int] */
    };

    this.link = inputs["link"];

    var outputs = {
        "shortest": new rxflow.ObservableSink() /* output shortest, [from: string, to: string, nxt: string, cost: int] */
    };

    this.shortest = outputs["shortest"].output;

    var elements = {
        0: new rxflow.Map(
            function(x) { return [x[0], x[1], x[1], x[2]]; /* [l.from, l.to, l.to, l.cost] */ }
        ),
        2: new rxflow.ObservableScanner(inputs["link"]),
        4: new rxflow.TableScanner(tables["path"]),
        5: new rxflow.HashJoin(
            function(x) { return x[1]; /* link.to */ },
            function(x) { return x[0]; /* path.from */ },
            "left"
        ),
        6: new rxflow.HashJoin(
            function(x) { return x[1]; /* link.to */ },
            function(x) { return x[0]; /* path.from */ },
            "right"
        ),
        7: new rxflow.Map(
            function(x) { return [x[0][0], x[1][1], x[0][1], x[0][2] + x[1][3]]; /* [l.from, p.to, l.to, l.cost + p.cost] */ }
        ),
        8: new rxflow.ArgMin(
            function(x) { return [x[0], x[1]]; /* [path.from, path.to] */ },
            function(x) { return x[3]; /* path.cost */ },
            function(x, y) { return x <= y; }
        )
    };

    var invalidationLookupTable = {
        "link": [elements[5], elements[6]],
        "path": [elements[5], elements[6]],
        2: [tables["path"], elements[5], elements[6]],
        4: [tables["path"], elements[5], elements[6], elements[8]],
        5: [tables["path"], elements[6]],
        6: [tables["path"], elements[5]],
        8: []
    };

    var rescanLookupTable = {
        "link": [elements[2], elements[4]],
        "path": [elements[2], elements[4]],
        2: [elements[4]],
        4: [elements[2]],
        5: [elements[2], elements[4]],
        6: [elements[2], elements[4]],
        8: []
    };

    elements[0].output.subscribe(tables["path"].insert);
    elements[2].output.subscribe(elements[0].input);
    elements[2].output.subscribe(elements[5].leftInput);
    elements[2].output.subscribe(elements[6].leftInput);
    elements[4].output.subscribe(elements[5].rightInput);
    elements[4].output.subscribe(elements[6].rightInput);
    elements[4].output.subscribe(elements[8].input);
    elements[5].output.subscribe(elements[7].input);
    elements[6].output.subscribe(elements[7].input);
    elements[7].output.subscribe(tables["path"].insert);
    elements[8].output.subscribe(outputs["shortest"].input);

    this.tick = function() {
        elements[2].endRound(currentTick);
        tables["path"].endRound(currentTick);
        currentTick += 1;
    }
}

var bloom = new Bloom();

bloom.shortest.subscribe(function(x) { console.log(x)});
bloom.link.onNext(['a', 'b', 1]);
bloom.link.onNext(['a', 'b', 4]);
bloom.link.onNext(['b', 'c', 1]);
bloom.link.onNext(['c', 'd', 1]);
bloom.link.onNext(['d', 'e', 1]);
bloom.tick();
console.log('----');
bloom.link.onNext(['e', 'f', 1]);
bloom.tick();
