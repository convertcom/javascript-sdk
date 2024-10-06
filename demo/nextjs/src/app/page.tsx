'use client';
import { useConvert } from '@/context/ConvertContext';

export default function Home() {
 const {  userId} = useConvert();
 console.log( userId);
  return (
    <div className="p-8">
      <h1 className="text-2xl font-bold">Home Page</h1>
      <p>Welcome to the home page.</p>
    </div>
  );
}