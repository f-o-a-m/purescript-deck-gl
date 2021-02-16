var DeckGL = require('deck.gl');

exports.makeTripsLayer = function (props) {
    return new DeckGL.TripsLayer(props);
};

exports.defaultTripsProps = DeckGL.TripsLayer.defaultProps;
