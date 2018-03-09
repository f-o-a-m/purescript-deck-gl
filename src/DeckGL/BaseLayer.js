exports.getLng = function (latLng) {
    return latLng[0];
};

exports.getLat = function (latLng) {
    return latLng[1];
};

exports.getElev = function (latLng) {
    return latLng[2];
};

exports.makeLngLatElev = function (lng) {
    return function (lat) {
        return function (elev) {
            return [lng, lat, elev];
        };
    };
};

exports.getPosLng = function (latLng) {
    return latLng[0];
};

exports.getPosLat = function (latLng) {
    return latLng[1];
};

exports.makePosition = function (lng) {
    return function (lat) {
        return [lng, lat];
    };
};
