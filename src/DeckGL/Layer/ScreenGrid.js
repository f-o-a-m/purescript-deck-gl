import { ScreenGridLayer } from 'deck.gl';

export const makeScreenGridLayer = function (props) {
    return new ScreenGridLayer(props);
};

export const defaultScreenGridProps = ScreenGridLayer.defaultProps;
