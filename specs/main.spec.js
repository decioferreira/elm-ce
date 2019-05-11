const evancz = require("../tmp/specs-elm-evancz");
const communityEdition = require("../tmp/specs-elm-ce");

const bothToBe = (fn, result) => {
  expect(fn(evancz)).toBe(result);
  expect(fn(communityEdition)).toBe(result);
};

test("onePlusOne to equal 2", () => {
  bothToBe((elm) => elm.onePlusOne, 2);
});

test("addOne(2) to equal 3", () => {
  bothToBe((elm) => elm.addOne(2), 3);
});
