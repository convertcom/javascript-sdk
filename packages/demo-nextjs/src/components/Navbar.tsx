'use client';
import { useState } from 'react';
import Link from 'next/link';

function Navbar() {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <nav className="bg-white shadow-md fixed w-full z-10 top-0">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <div className="flex-shrink-0">
            <Link href="/">
              <span className="text-xl font-bold text-gray-800">MyApp</span>
            </Link>
          </div>

          {/* Desktop Menu */}
          <div className="hidden md:block">
            <div className="ml-10 flex items-baseline space-x-4">
              <Link href="/">
                <span className="text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium">Home</span>
              </Link>
              <Link href="/about">
                <span className="text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium">About</span>
              </Link>
              <Link href="/services">
                <span className="text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium">Services</span>
              </Link>
              <Link href="/contact">
                <span className="text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-sm font-medium">Contact</span>
              </Link>
            </div>
          </div>

          {/* Mobile Menu Button */}
          <div className="md:hidden">
            <button
              onClick={() => setIsOpen(!isOpen)}
              type="button"
              className="text-gray-800 hover:text-gray-600 focus:outline-none focus:text-gray-600"
              aria-label="toggle menu"
            >
              <svg viewBox="0 0 24 24" className="h-6 w-6 fill-current">
                {isOpen ? (
                  <path
                    fillRule="evenodd"
                    clipRule="evenodd"
                    d="M18.3 5.71L12 12l6.3 6.29-1.42 1.42L12 14.84l-6.29 6.29-1.42-1.42L10.58 12 4.29 5.71 5.71 4.29 12 10.58l6.29-6.29 1.42 1.42z"
                  />
                ) : (
                  <path
                    fillRule="evenodd"
                    d="M4 5h16v2H4V5zm0 6h16v2H4v-2zm0 6h16v2H4v-2z"
                  />
                )}
              </svg>
            </button>
          </div>
        </div>
      </div>

      {/* Mobile Menu */}
      {isOpen && (
        <div className="md:hidden">
          <div className="px-2 pt-2 pb-3 space-y-1 sm:px-3">
            <Link href="/">
              <span className="block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium">Home</span>
            </Link>
            <Link href="/about">
              <span className="block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium">About</span>
            </Link>
            <Link href="/services">
              <span className="block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium">Services</span>
            </Link>
            <Link href="/contact">
              <span className="block text-gray-800 hover:text-gray-600 px-3 py-2 rounded-md text-base font-medium">Contact</span>
            </Link>
          </div>
        </div>
      )}
    </nav>
  );
}

export default Navbar;
