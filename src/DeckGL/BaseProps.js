exports.getLng = function (latLng) {
    return latLng[0];
};

exports.getLat = function (latLng) {
    return latLng[1];
};

exports.getElev = function (latLng) {
    return latLng[2];
};

exports.makeLngLat = function (lng) {
    return function (lat) {
        return [lng, lat, 0];
    };
};
