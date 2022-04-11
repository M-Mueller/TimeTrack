var path = require("path");
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
    mode: "development",
    entry: "./src/Client.fs.js",
    output: {
        filename: "bundle.js",
        path: path.join(__dirname, "public"),
    },
    devServer: {
        static: [{
            directory: path.join(__dirname, "public"),
        }, {
            directory: path.join(__dirname, "node_modules/doodle.css"),
            publicPath: '/css',
        }],
        port: 8000
    },
    plugins: [
        new CopyPlugin({
            patterns: [
                {
                    from: "*.{css,svg}",
                    context: path.join(__dirname, "node_modules/doodle.css/").replace(/\\/g, "/"),
                    to: path.join(__dirname, "public/css")
                },
            ],
        }),
    ],
}