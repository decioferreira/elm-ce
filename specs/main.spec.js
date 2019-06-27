const evancz = require("../tmp/specs-elm-evancz");
const communityEdition = require("../tmp/specs-elm-ce");

const evanczApp = evancz.Elm.Main.init();
const ceApp = communityEdition.Elm.Main.init();

const runBoth = params => {
  Promise.all([
    new Promise((resolve, reject) => {
      evanczApp.ports[params.outgoingPort].subscribe(evanczResult => {
        params.callback(evanczResult, resolve, reject);
      });
    }),
    new Promise((resolve, reject) => {
      ceApp.ports[params.outgoingPort].subscribe(ceResult => {
        params.callback(ceResult, resolve, reject);
      });
    })
  ]).then(() => params.done());

  evanczApp.ports[params.incomingPort].send(params.sendParam);
  ceApp.ports[params.incomingPort].send(params.sendParam);
};

// NEW
test("onePlusOne to equal 2", done => {
  runBoth({
    incomingPort: "incomingOnePlusOne",
    outgoingPort: "outgoingOnePlusOne",
    sendParam: null,
    callback: (result, resolve) => {
      expect(result).toBe(2);
      resolve();
    },
    done: done
  });
});

test("addOne(2) to equal 3", done => {
  runBoth({
    incomingPort: "incomingAddOne",
    outgoingPort: "outgoingAddOne",
    sendParam: 2,
    callback: (result, resolve) => {
      expect(result).toBe(3);
      resolve();
    },
    done: done
  });
});
