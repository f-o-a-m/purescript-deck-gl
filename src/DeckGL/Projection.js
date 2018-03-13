const WMViewport = require('deck.gl').WebMercatorViewport;

exports.makeMercatorProjector = function (vp) {
    return new WMViewport(vp);
};

exports.unproject = function (projector, screenCoords) {
    return projector.unproject([screenCoords.x, screenCoords.y]);
};

exports.project = function (projector, lngLat) {
    const xy = projector.project(lngLat);
    return {x: xy[0], y: xy[1]};
};
