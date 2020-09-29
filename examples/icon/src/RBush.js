const RBush = require("rbush");

exports.empty = function (maxInNode) {
  class MyRBush extends RBush {
    toBBox({ x, y }) { return {minX: x, minY: y, maxX: x, maxY: y}; }
    compareMinX(a, b) { return a.x - b.x; }
    compareMinY(a, b) { return a.y - b.y; }
  }

  return new MyRBush(maxInNode);
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
