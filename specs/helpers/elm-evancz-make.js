#!/usr/bin/env node
"use strict";

const childProcess = require("child_process");
const program = require("commander");

program.parse(process.argv);

const files = program.args;
const outputPath = program.output || "/dev/null";

const child = childProcess.exec(`elm make ${files.join(" ")} --output=${outputPath}`);
child.stdout.pipe(process.stdout);
child.stderr.pipe(process.stderr);
child.on("exit", function(code) {
  process.exit(code);
});
