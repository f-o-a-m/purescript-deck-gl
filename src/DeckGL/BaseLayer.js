exports.getLat = function (latLng) {
    return latLng[1];
};

exports.getLng = function (latLng) {
    return latLng[0];
};

exports.mkLngLat = function (lng) {
    return function (lat) {
        return [lng, lat];
    };
};

exports.mkLngLatElev = function (lng) {
    return function (lat) {
        return function (elev) {
            return [lng, lat, elev];
        };
    };
};
