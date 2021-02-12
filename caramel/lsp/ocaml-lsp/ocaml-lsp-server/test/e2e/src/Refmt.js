#!/usr/bin/env node

var chunks = [];
process.stdin.resume();
process.stdin.on("data", function (chunk) {
  chunks.push(chunk);
});
process.stdin.on("end", function () {
  var body = Buffer.concat(chunks);
  if (body == "special string") {
    process.stderr.write("special string passed\n");
    process.exit(1);
  } else {
    process.stdout.write(body);
  }
});
