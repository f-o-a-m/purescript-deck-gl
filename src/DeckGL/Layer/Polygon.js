import { PolygonLayer } from 'deck.gl';

export const makePolygonLayer = function (props) {
    return new PolygonLayer(props);
};

export const defaultPolygonProps = PolygonLayer.defaultProps;
