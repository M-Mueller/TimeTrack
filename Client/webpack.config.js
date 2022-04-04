var path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/Client.fs.js",
    output: {
        filename: "bundle.js",
        path: path.join(__dirname, "public"),
    },
    devServer: {
        static: {
            directory: path.join(__dirname, "public")
        },
        port: 8000
    }
}