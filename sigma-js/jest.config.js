/** @type {import("jest").Config} */
const config = {
  moduleDirectories: ["<rootDir>/node_modules"],
  moduleNameMapper: {
    "sigmastate-js": "<rootDir>/../sdk/js/target/scala-2.13/sdk-fastopt/main.js",
  },
};

module.exports = config;
