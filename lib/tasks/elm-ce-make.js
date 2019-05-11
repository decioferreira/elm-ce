#!/usr/bin/env node
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
const outputPath = program.output || "index.js";

const app = Elm.Main.init({ flags: content });


new Promise(function(resolve, reject) {
  app.ports.converted.subscribe(function(output) {
    fs.writeFileSync(outputPath, output);
    resolve();
  });

  app.ports.convert.send(content);
});
