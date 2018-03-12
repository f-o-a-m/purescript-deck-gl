const DeckGL = require("deck.gl");

exports.makePolygonLayer = function (props) {
    return new DeckGL.PolygonLayer(props);
};

exports.defaultPolygonProps = DeckGL.PolygonLayer.defaultProps;
