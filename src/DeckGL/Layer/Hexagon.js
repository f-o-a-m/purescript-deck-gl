var DeckGL = require('deck.gl');

exports.makeHexagonLayer = function (props) {
    return new DeckGL.HexagonLayer(props);
};

exports.defaultHexagonProps = DeckGL.HexagonLayer.defaultProps;
