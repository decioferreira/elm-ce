#!/usr/bin/env node
"use strict";

const childProcess = require("child_process");
const program = require("commander");

program
  .version("0.0.1", "-v, --version")
  .usage("<zero-or-more-elm-files> [options]")
  .option(
    "-o, --output <output-file>",
    "Specify the name of the resulting JS file."
  )
  .parse(process.argv);

const files = program.args;
const outputPath = program.output || "/dev/null";

childProcess.exec(
  `elm make ${files.join(" ")} --output=${outputPath}`,
  function(error, stdout, stderr) {
    if (stdout) {
      console.log(stdout);
    }

    if (stderr) {
      console.error(stderr);
      process.exit(error.code);
    }

  }
);
