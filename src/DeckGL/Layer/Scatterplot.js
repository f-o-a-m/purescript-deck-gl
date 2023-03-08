import { ScatterplotLayer } from 'deck.gl';

export const makeScatterplotLayer = function (props) {
    return new ScatterplotLayer(props);
};

export const defaultScatterplotProps = ScatterplotLayer.defaultProps;
