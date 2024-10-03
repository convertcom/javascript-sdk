import React, { useState } from "react";
import { ReactComponent as ChevronRightIcon } from "feather-icons/dist/icons/chevron-right.svg";
import tw from "twin.macro";

const ProfilePicture = tw.img`rounded-full w-8 h-8`;

const defaultItem = "Select a user";

export default ({ data = [], selected = null, setSelected = () => {} }) => {
  const [isOpen, setOpen] = useState(false);
  const [items] = useState(data);
  const [selectedItem, setSelectedItem] = useState(selected);

  const toggleDropdown = () => setOpen(!isOpen);

  const handleItemClick = (id) => {
    selectedItem === id ? setSelectedItem(null) : setSelectedItem(id);
    setSelected(id);
    toggleDropdown();
  };

  return (
    <div className="dropdown">
      <div className="dropdown-header" onClick={toggleDropdown}>
        {selectedItem && (
          <ProfilePicture
            src={items.find((item) => item.email === selectedItem)?.photo}
            alt={items.find((item) => item.email === selectedItem)?.name}
          />
        )}
        {selectedItem
          ? items.find((item) => item.email === selectedItem)?.name ||
            defaultItem
          : defaultItem}
        <i className={`icon ${isOpen && "open"}`}>
          <ChevronRightIcon />
        </i>
      </div>
      <div className={`dropdown-body ${isOpen && "open"}`}>
        {items.map((item) => (
          <div
            key={item.email}
            className="dropdown-item"
            onClick={(e) => handleItemClick(e.target.id)}
            id={item.email}
          >
            <span
              className={`dropdown-item-dot ${
                item.email === selectedItem && "selected"
              }`}
            >
              •{" "}
            </span>
            {item.name}
          </div>
        ))}
      </div>
    </div>
  );
};
