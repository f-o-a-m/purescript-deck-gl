var DeckGL = require('deck.gl');

exports.defaultIconProps = DeckGL.IconLayer.defaultProps;

exports.makeIconLayer = function (props) {
    return new DeckGL.IconLayer(props);
};
