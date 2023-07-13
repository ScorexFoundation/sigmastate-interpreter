const fs = require("fs");
const pkg = require("../package.json");
const gitHash = require("child_process")
  .execSync("git rev-parse --short HEAD")
  .toString()
  .trim();

pkg.version = `${pkg.version}-snapshot.${gitHash}`;
fs.writeFileSync("./package.json", JSON.stringify(pkg, null, 2));
