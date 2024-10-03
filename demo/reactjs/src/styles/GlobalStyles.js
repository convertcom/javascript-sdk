import { createGlobalStyle } from "styled-components";
import { globalStyles } from "twin.macro";

const GlobalStyles = createGlobalStyle(
  globalStyles,
  `
  /* Below animations are for modal created using React-Modal */
    .ReactModal__Overlay {
    transition: transform 300ms ease-in-out;
    transition-delay: 100ms;
    transform: scale(0);
  }
  .ReactModal__Overlay--after-open{
    transform: scale(1);
  }
  .ReactModal__Overlay--before-close{
    transform: scale(0);
  }

  .dropdown-wrapper {
    position: absolute;
  }
  .dropdown {
    width: 200px;
    border-radius: 10px;
    box-shadow: 0 10px 25px rgba(0,0,0,.1);
    background-color: white;
  }
  @media (max-width: 884px) {
    .dropdown {
      width: 100%;
    }
  }
  .dropdown-header {
    padding: 5px;
    cursor: pointer;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .dropdown-body {
    padding: 5px;
    border-top: 1px solid #E5E8EC;
    display: none;
  }
  .dropdown-body.open {
    display: block;
  }
  .dropdown-item {
    padding: 10px;
  }
  .dropdown-item:hover {
    cursor: pointer;
  }
  .dropdown-item-dot {
    opacity: 0;
    color: #91A5BE;
    transition: all .2s ease-in-out;
  }
  .dropdown-item-dot.selected {
    opacity: 1;
  }
  .dropdown .icon {
    font-size: 13px;
    color: #91A5BE;
    transform: rotate(0deg);
    transition: all .2s ease-in-out;
  }
  .dropdown .icon.open {
    transform: rotate(90deg);
  }
  `
);

export default GlobalStyles;
