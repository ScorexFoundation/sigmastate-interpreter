/** @type {import("jest").Config} */
const config = {
  moduleDirectories: ["<rootDir>/node_modules"],
  moduleNameMapper: {
    "sigmastate-js": "<rootDir>/../sc/js/target/scala-2.13/sc-fastopt/main.js",
  },
};

module.exports = config;
