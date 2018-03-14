const WMViewport = require('deck.gl').WebMercatorViewport;

exports.makeMercatorProjector = function (vp) {
    const mp = new WMViewport(vp);
    return mp;
};

exports.unproject = function (projector, screenCoords) {
    return projector.unproject([screenCoords.x, screenCoords.y]);
};

exports.project = function (projector, lngLat) {
    console.log(projector);
    console.log(lngLat);
    const xy = projector.project(lngLat);
    return {x: xy[0], y: xy[1]};
};
