import { ArcLayer } from 'deck.gl';

export const makeArcLayer = function (props) {
    return new ArcLayer(props);
};

export const defaultArcProps = ArcLayer.defaultProps;
