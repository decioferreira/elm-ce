#!/usr/bin/env node
"use strict";

const { Elm } = require("../../tmp/elm-ce");
const fs = require("fs");
const program = require("commander");

program
  .version("0.0.1", "-v, --version")
  .usage("<zero-or-more-elm-files> [options]")
  .option(
    "-o, --output <output-file>",
    "Specify the name of the resulting JS file."
  )
  .parse(process.argv);

const [file] = program.args;
const content = fs.readFileSync(file, "utf8");
const options = program.opts();
const outputPath = options.output || "/dev/null";
const elm = fs.readFileSync("lib/elm.js", "utf8");

const app = Elm.Main.init({ flags: elm });

new Promise(function(resolve, reject) {
  app.ports.convertedSuccess.subscribe(function(output) {
    fs.writeFileSync(outputPath, output.code);
    console.log(output.message);
    resolve();
  });

  app.ports.convertedFail.subscribe(function(output) {
    console.error(output.error);
    console.log(output.message);
    process.exit(1);
    resolve();
  });

  app.ports.convert.send(content);
});
