import React from 'react';

import Header, {NavLink, NavLinks} from '../headers/light.js';

export default () => {
  const navLinks = [
    <NavLinks key={1} style={{minHeight: '42px'}}>
      <NavLink href="/events">Events</NavLink>
      <NavLink href="/statistics">Statistics</NavLink>
      <NavLink href="/pricing">Pricing</NavLink>
    </NavLinks>
  ];

  return (
    <>
      <Header links={navLinks} />
    </>
  );
};
