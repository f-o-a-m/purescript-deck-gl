const RBush = require("rbush");

exports.empty = function (maxInNode) {
    return new RBush(maxInNode, ['.x', '.y', '.x', '.y']);
};

exports.insert = function (a) {
    return function (bush) {
        bush.insert(a);
        return bush;
    };
};

exports.insertMany = function (as) {
    return function (bush) {
        bush.load(as);
        return bush;
    };
};

exports.search = function (box) {
    return function (bush) {
        return bush.search(box);
    };
};
