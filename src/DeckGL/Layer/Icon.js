import { IconLayer } from 'deck.gl';

export const defaultIconProps = IconLayer.defaultProps;

export const makeIconLayer = function (props) {
    return new IconLayer(props);
};
