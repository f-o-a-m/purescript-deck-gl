import { HexagonLayer } from 'deck.gl';

export const makeHexagonLayer = function (props) {
    return new HexagonLayer(props);
};

export const defaultHexagonProps = HexagonLayer.defaultProps;
