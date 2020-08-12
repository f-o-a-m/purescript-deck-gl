var DeckGL = require('deck.gl');

exports.makeScreenGridLayer = function (props) {
    return new DeckGL.ScreenGridLayer(props);
};

exports.defaultScreenGridProps = DeckGL.ScreenGridLayer.defaultProps;