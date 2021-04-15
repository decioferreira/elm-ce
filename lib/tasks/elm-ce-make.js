#!/usr/bin/env node
"use strict";

const { Elm } = require("../../tmp/elm-ce");
const fs = require("fs");
const glob = require("glob");
const path = require("path");
const program = require("commander");

const libraries = [["elm", "core"]];
const librarySourceFiles = libraries.reduce((acc, library) => {
  const libraryFiles = glob.sync(`${__dirname}/../libraries/${library.join("/")}/**/*.elm`);

  return libraryFiles.reduce((fileAcc, file) => {
    fileAcc.push({
      namespace: library,
      source: fs.readFileSync(file, "utf8"),
    });
    return fileAcc;
  }, acc);
}, []);

const elmJson = require(path.join(process.cwd(), "elm.json"));
const authorProjectFiles = glob.sync(
  `${elmJson["source-directories"].join("|")}/**/*.elm`
);


const sourceFiles = authorProjectFiles.reduce((acc, file) => {
  acc.push({
    namespace: ["author", "project"],
    source: fs.readFileSync(file, "utf8"),
  });
  return acc;
}, librarySourceFiles);

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

// TODO read JS files from elm/core (and other libraries...)
// const elmCore = fs.readFileSync("../../lib/elm.js", "utf8");
const elmCore = "";

const app = Elm.Main.init({ flags: { elmCore, sourceFiles } });

new Promise(function (resolve, reject) {
  app.ports.convertedSuccess.subscribe(function (output) {
    fs.writeFileSync(outputPath, output.code);
    console.log(output.message);
    resolve();
  });

  app.ports.convert.send(null);
});
