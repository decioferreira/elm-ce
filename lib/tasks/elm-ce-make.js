#!/usr/bin/env node
"use strict";

const { Elm } = require("../../tmp/elm-ce");
const fs = require("fs");
const glob = require("glob");
const path = require("path");
const program = require("commander");

const elmJson = require(path.join(process.cwd(), "elm.json"));
const files = glob.sync(`${elmJson["source-directories"].join("|")}/**/*.elm`);

const sourceFiles = files.reduce((acc, file) => {
  acc.push({
    namespace: ["author", "project"],
    path: file,
    source: fs.readFileSync(file, "utf8"),
  });
  return acc;
}, []);

program
  .version("0.0.1", "-v, --version")
  .usage("<zero-or-more-elm-files> [options]")
  .option(
    "-o, --output <output-file>",
    "Specify the name of the resulting JS file."
  )
  .parse(process.argv);

const [file] = program.args;
const options = program.opts();
const outputPath = options.output || "/dev/null";
const elmCore = fs.readFileSync("../../lib/elm.js", "utf8");

const app = Elm.Main.init({ flags: { elmCore, sourceFiles } });

new Promise(function (resolve, reject) {
  app.ports.convertedSuccess.subscribe(function (output) {
    fs.writeFileSync(outputPath, output.code);
    console.log(output.message);
    resolve();
  });

  app.ports.convert.send(null);
});
