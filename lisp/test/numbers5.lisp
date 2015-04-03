
;;; ASH
(defparameter ash-results
  ;; This append nonsense is an attempt to work around a still unresolve eclipse bug
  (append
   '(#x1 #x1 #x2 #x0 #x4 #x0 #x8 #x0 #x10 #x0 #x1000 #x0 #x2000 #x0 #x4000 #x0
	 #x8000 #x0 #x10000 #x0 #x20000 #x0 #x40000 #x0 #x2000000 #x0 #x4000000 #x0
	 #x8000000 #x0 #x10000000 #x0 #x20000000 #x0 #x40000000 #x0 #x80000000 #x0 #x100000000
	 #x0 #x200000000 #x0 #x400000000 #x0 #x4000000000000 #x0 #x-1 #x-1 #x-2)
   '(#x-1 #x-4
	  #x-1 #x-8 #x-1 #x-10 #x-1 #x-1000 #x-1 #x-2000 #x-1 #x-4000 #x-1 #x-8000 #x-1
	  #x-10000 #x-1 #x-20000 #x-1 #x-40000 #x-1 #x-2000000 #x-1 #x-4000000 #x-1 #x-8000000
	  #x-1 #x-10000000 #x-1 #x-20000000 #x-1 #x-40000000 #x-1 #x-80000000 #x-1 #x-100000000
	  #x-1 #x-200000000 #x-1 #x-400000000 #x-1 #x-4000000000000 #x-1 #x5 #x5 #xa #x2 #x14
	  #x1)
   '(#x28 #x0 #x50 #x0 #x5000 #x0 #xa000 #x0 #x14000 #x0 #x28000 #x0 #x50000
	  #x0 #xa0000 #x0 #x140000 #x0 #xa000000 #x0 #x14000000 #x0 #x28000000 #x0 #x50000000
	  #x0 #xa0000000 #x0 #x140000000 #x0 #x280000000 #x0 #x500000000 #x0 #xa00000000 #x0
	  #x1400000000 #x0 #x14000000000000 #x0 #x-5 #x-5 #x-a #x-3 #x-14 #x-2 #x-28 #x-1
	  #x-50)
   '(#x-1 #x-5000 #x-1 #x-a000 #x-1 #x-14000 #x-1 #x-28000 #x-1 #x-50000 #x-1
	  #x-a0000 #x-1 #x-140000 #x-1 #x-a000000 #x-1 #x-14000000 #x-1 #x-28000000 #x-1
	  #x-50000000 #x-1 #x-a0000000 #x-1 #x-140000000 #x-1 #x-280000000 #x-1 #x-500000000 #x-1
	  #x-a00000000 #x-1 #x-1400000000 #x-1 #x-14000000000000 #x-1 #x9 #x9 #x12 #x4 #x24
	  #x2 #x48 #x1 #x90 #x0 #x9000 #x0)
   '(#x12000 #x0 #x24000 #x0 #x48000 #x0 #x90000
	     #x0 #x120000 #x0 #x240000 #x0 #x12000000 #x0 #x24000000 #x0 #x48000000 #x0
	     #x90000000 #x0 #x120000000 #x0 #x240000000 #x0 #x480000000 #x0 #x900000000 #x0
	     #x1200000000 #x0 #x2400000000 #x0 #x24000000000000 #x0 #x-9 #x-9 #x-12 #x-5 #x-24
	     #x-3 #x-48 #x-2 #x-90 #x-1 #x-9000 #x-1 #x-12000 #x-1 #x-24000)
   '(#x-1 #x-48000 #x-1
	  #x-90000 #x-1 #x-120000 #x-1 #x-240000 #x-1 #x-12000000 #x-1 #x-24000000 #x-1
	  #x-48000000 #x-1 #x-90000000 #x-1 #x-120000000 #x-1 #x-240000000 #x-1 #x-480000000 #x-1
	  #x-900000000 #x-1 #x-1200000000 #x-1 #x-2400000000 #x-1 #x-24000000000000 #x-1 #x7fff
	  #x7fff #xfffe #x3fff #x1fffc #x1fff #x3fff8 #xfff #x7fff0 #x7ff #x7fff000 #x7
	  #xfffe000 #x3 #x1fffc000 #x1 #x3fff8000 #x0)
   '(#x7fff0000 #x0 #xfffe0000 #x0 #x1fffc0000
		#x0 #xfffe000000 #x0 #x1fffc000000 #x0 #x3fff8000000 #x0 #x7fff0000000 #x0
		#xfffe0000000 #x0 #x1fffc0000000 #x0 #x3fff80000000
		#x0 #x7fff00000000 #x0 #xfffe00000000 
		#x0 #x1fffc00000000 #x0 #x1fffc000000000000 #x0 #x-7fff #x-7fff #x-fffe #x-4000
		#x-1fffc #x-2000 #x-3fff8 #x-1000 #x-7fff0 #x-800 #x-7fff000 #x-8 #x-fffe000 #x-4
		#x-1fffc000 #x-2 #x-3fff8000 #x-1 #x-7fff0000 #x-1 #x-fffe0000)
   '(#x-1 #x-1fffc0000 #x-1
	  #x-fffe000000 #x-1 #x-1fffc000000 #x-1 #x-3fff8000000 #x-1 #x-7fff0000000 #x-1
	  #x-fffe0000000 #x-1 #x-1fffc0000000 #x-1 #x-3fff80000000 #x-1 #x-7fff00000000 #x-1
	  #x-fffe00000000 #x-1 #x-1fffc00000000 #x-1 #x-1fffc000000000000 #x-1 #xffff #xffff
	  #x1fffe #x7fff #x3fffc #x3fff #x7fff8 #x1fff #xffff0 #xfff #xffff000 #xf #x1fffe000
	  #x7 #x3fffc000 #x3 #x7fff8000 #x1 #xffff0000 #x0 #x1fffe0000 #x0 #x3fffc0000 #x0)
   '(#x1fffe000000 #x0 #x3fffc000000 #x0 #x7fff8000000 #x0 #xffff0000000 #x0 #x1fffe0000000
		   #x0 #x3fffc0000000 #x0 #x7fff80000000 #x0 #xffff00000000 #x0 #x1fffe00000000 #x0
		   #x3fffc00000000 #x0 #x3fffc000000000000 #x0 #x-ffff
		   #x-ffff #x-1fffe #x-8000 #x-3fffc 
		   #x-4000 #x-7fff8 #x-2000 #x-ffff0 #x-1000
		   #x-ffff000 #x-10 #x-1fffe000 #x-8 #x-3fffc000 
		   #x-4 #x-7fff8000 #x-2 #x-ffff0000 #x-1 #x-1fffe0000 #x-1 #x-3fffc0000 #x-1
		   #x-1fffe000000 #x-1 #x-3fffc000000)
   '(#x-1 #x-7fff8000000 #x-1 #x-ffff0000000 #x-1
	  #x-1fffe0000000 #x-1 #x-3fffc0000000 #x-1 #x-7fff80000000 #x-1 #x-ffff00000000 #x-1
	  #x-1fffe00000000 #x-1 #x-3fffc00000000 #x-1 #x-3fffc000000000000 #x-1 #x1ffff #x1ffff
	  #x3fffe #xffff #x7fffc #x7fff #xffff8 #x3fff #x1ffff0 #x1fff #x1ffff000 #x1f
	  #x3fffe000 #xf #x7fffc000 #x7 #xffff8000 #x3 #x1ffff0000 #x1 #x3fffe0000 #x0
	  #x7fffc0000 #x0 #x3fffe000000 #x0 #x7fffc000000 #x0 #xffff8000000 #x0)
   '(#x1ffff0000000
     #x0 #x3fffe0000000 #x0 #x7fffc0000000 #x0 #xffff80000000 #x0 #x1ffff00000000 #x0
     #x3fffe00000000 #x0 #x7fffc00000000 #x0 #x7fffc000000000000 #x0 #x-1ffff #x-1ffff
     #x-3fffe #x-10000 #x-7fffc #x-8000 #x-ffff8 #x-4000 #x-1ffff0 #x-2000 #x-1ffff000 #x-20
     #x-3fffe000 #x-10 #x-7fffc000 #x-8 #x-ffff8000 #x-4 #x-1ffff0000 #x-2 #x-3fffe0000 #x-1
     #x-7fffc0000 #x-1 #x-3fffe000000 #x-1 #x-7fffc000000 #x-1 #x-ffff8000000 #x-1
     #x-1ffff0000000 #x-1 #x-3fffe0000000)
   '(#x-1 #x-7fffc0000000 #x-1 #x-ffff80000000 #x-1
	  #x-1ffff00000000 #x-1 #x-3fffe00000000 #x-1 #x-7fffc00000000 #x-1 #x-7fffc000000000000 #x-1
	  #xfffff #xfffff #x1ffffe #x7ffff #x3ffffc #x3ffff #x7ffff8 #x1ffff #xfffff0 #xffff
	  #xfffff000 #xff #x1ffffe000 #x7f #x3ffffc000 #x3f #x7ffff8000 #x1f #xfffff0000 #xf
	  #x1ffffe0000 #x7 #x3ffffc0000 #x3 #x1ffffe000000 #x0 #x3ffffc000000 #x0 #x7ffff8000000
	  #x0 #xfffff0000000 #x0 #x1ffffe0000000 #x0 #x3ffffc0000000 #x0)
   '(#x7ffff80000000 #x0 #xfffff00000000 #x0 #x1ffffe00000000 #x0
		     #x3ffffc00000000 #x0 #x3ffffc000000000000 #x0
		     #x-fffff #x-fffff #x-1ffffe #x-80000 #x-3ffffc
		     #x-40000 #x-7ffff8 #x-20000 #x-fffff0 #x-10000
		     #x-fffff000 #x-100 #x-1ffffe000 #x-80
		     #x-3ffffc000 #x-40 #x-7ffff8000 #x-20
		     #x-fffff0000 #x-10 #x-1ffffe0000 #x-8
		     #x-3ffffc0000 #x-4 #x-1ffffe000000 #x-1
		     #x-3ffffc000000 #x-1 #x-7ffff8000000 #x-1
		     #x-fffff0000000 #x-1 #x-1ffffe0000000 #x-1
		     #x-3ffffc0000000 #x-1 #x-7ffff80000000 #x-1
		     #x-fffff00000000) 
   '(#x-1 #x-1ffffe00000000 #x-1
	  #x-3ffffc00000000 #x-1 #x-3ffffc000000000000 #x-1 #xf0001 #xf0001 #x1e0002 #x78000
	  #x3c0004 #x3c000 #x780008 #x1e000 #xf00010 #xf000 #xf0001000 #xf0 #x1e0002000 #x78
	  #x3c0004000 #x3c #x780008000 #x1e #xf00010000 #xf #x1e00020000 #x7 #x3c00040000 #x3
	  #x1e0002000000 #x0 #x3c0004000000 #x0 #x780008000000 #x0 #xf00010000000 #x0
	  #x1e00020000000 #x0 #x3c00040000000 #x0 #x7800080000000 #x0 #xf000100000000 #x0
	  #x1e000200000000 #x0)
   '(#x3c000400000000 #x0 #x3c0004000000000000 #x0 #x-f0001 #x-f0001
		      #x-1e0002 #x-78001 #x-3c0004 #x-3c001 #x-780008
		      #x-1e001 #x-f00010 #x-f001 #x-f0001000 #x-f1
		      #x-1e0002000 #x-79 #x-3c0004000 #x-3d
		      #x-780008000 #x-1f #x-f00010000 #x-10
		      #x-1e00020000 #x-8 #x-3c00040000 #x-4
		      #x-1e0002000000 #x-1 #x-3c0004000000 #x-1
		      #x-780008000000 #x-1 #x-f00010000000 #x-1
		      #x-1e00020000000 #x-1 #x-3c00040000000 #x-1
		      #x-7800080000000 #x-1 #x-f000100000000 #x-1
		      #x-1e000200000000 #x-1 #x-3c000400000000 #x-1 
		      #x-3c0004000000000000)
   '(#x-1 #x7fffffff #x7fffffff #xfffffffe #x3fffffff #x1fffffffc
	  #x1fffffff #x3fffffff8 #xfffffff #x7fffffff0 #x7ffffff #x7fffffff000 #x7ffff #xfffffffe000
	  #x3ffff #x1fffffffc000 #x1ffff #x3fffffff8000 #xffff #x7fffffff0000 #x7fff #xfffffffe0000
	  #x3fff #x1fffffffc0000 #x1fff #xfffffffe000000 #x3f #x1fffffffc000000 #x1f
	  #x3fffffff8000000 #xf #x7fffffff0000000 #x7 #xfffffffe0000000 #x3 #x1fffffffc0000000 #x1
	  #x3fffffff80000000 #x0 #x7fffffff00000000 #x0 #xfffffffe00000000 #x0 #x1fffffffc00000000
	  #x0 #x1fffffffc000000000000 #x0 #x-7fffffff #x-7fffffff)
   '(#x-fffffffe #x-40000000 #x-1fffffffc #x-20000000 #x-3fffffff8
		 #x-10000000 #x-7fffffff0 #x-8000000 #x-7fffffff000
		 #x-80000 #x-fffffffe000 #x-40000 #x-1fffffffc000
		 #x-20000 #x-3fffffff8000 #x-10000 #x-7fffffff0000
		 #x-8000 #x-fffffffe0000 #x-4000 #x-1fffffffc0000
		 #x-2000 #x-fffffffe000000 #x-40 #x-1fffffffc000000
		 #x-20 #x-3fffffff8000000 #x-10 #x-7fffffff0000000
		 #x-8 #x-fffffffe0000000 #x-4 #x-1fffffffc0000000 #x-2
		 #x-3fffffff80000000 #x-1 #x-7fffffff00000000 #x-1
		 #x-fffffffe00000000 #x-1 #x-1fffffffc00000000 #x-1
		 #x-1fffffffc000000000000 #x-1 #xffffffff #xffffffff
		 #x1fffffffe #x7fffffff #x3fffffffc) 
   '(#x3fffffff #x7fffffff8 #x1fffffff #xffffffff0 #xfffffff
		#xffffffff000 #xfffff #x1fffffffe000 #x7ffff
		#x3fffffffc000 #x3ffff #x7fffffff8000 #x1ffff
		#xffffffff0000 #xffff #x1fffffffe0000 #x7fff
		#x3fffffffc0000 #x3fff #x1fffffffe000000 #x7f
		#x3fffffffc000000 #x3f #x7fffffff8000000 #x1f
		#xffffffff0000000 #xf #x1fffffffe0000000 #x7
		#x3fffffffc0000000 #x3 #x7fffffff80000000 #x1
		#xffffffff00000000 #x0 #x1fffffffe00000000 #x0
		#x3fffffffc00000000 #x0 #x3fffffffc000000000000 #x0
		#x-ffffffff #x-ffffffff #x-1fffffffe #x-80000000
		#x-3fffffffc #x-40000000 #x-7fffffff8 #x-20000000) 
   '(#x-ffffffff0 #x-10000000 #x-ffffffff000 #x-100000 #x-1fffffffe000 #x-80000 #x-3fffffffc000
		  #x-40000 #x-7fffffff8000 #x-20000 #x-ffffffff0000 #x-10000 #x-1fffffffe0000 #x-8000
		  #x-3fffffffc0000 #x-4000 #x-1fffffffe000000 #x-80
		  #x-3fffffffc000000 #x-40 #x-7fffffff8000000 #x-20
		  #x-ffffffff0000000 #x-10 #x-1fffffffe0000000 #x-8
		  #x-3fffffffc0000000 #x-4 #x-7fffffff80000000 #x-2
		  #x-ffffffff00000000 #x-1 #x-1fffffffe00000000 #x-1
		  #x-3fffffffc00000000 #x-1 #x-3fffffffc000000000000
		  #x-1 #xffffffffffff #xffffffffffff #x1fffffffffffe
		  #x7fffffffffff #x3fffffffffffc #x3fffffffffff
		  #x7fffffffffff8 #x1fffffffffff #xffffffffffff0
		  #xfffffffffff #xffffffffffff000) 
   '(#xfffffffff #x1fffffffffffe000 #x7ffffffff #x3fffffffffffc000
		 #x3ffffffff #x7fffffffffff8000 #x1ffffffff
		 #xffffffffffff0000 #xffffffff #x1fffffffffffe0000
		 #x7fffffff #x3fffffffffffc0000 #x3fffffff
		 #x1fffffffffffe000000 #x7fffff #x3fffffffffffc000000
		 #x3fffff #x7fffffffffff8000000 #x1fffff
		 #xffffffffffff0000000 #xfffff #x1fffffffffffe0000000
		 #x7ffff #x3fffffffffffc0000000 #x3ffff
		 #x7fffffffffff80000000 #x1ffff #xffffffffffff00000000
		 #xffff #x1fffffffffffe00000000 #x7fff
		 #x3fffffffffffc00000000 #x3fff
		 #x3fffffffffffc000000000000 #x0 #x-ffffffffffff
		 #x-ffffffffffff #x-1fffffffffffe #x-800000000000
		 #x-3fffffffffffc #x-400000000000 #x-7fffffffffff8
		 #x-200000000000 #x-ffffffffffff0 #x-100000000000
		 #x-ffffffffffff000 #x-1000000000 #x-1fffffffffffe000 
		 #x-800000000)
   '(#x-3fffffffffffc000 #x-400000000 #x-7fffffffffff8000 #x-200000000
			 #x-ffffffffffff0000 #x-100000000
			 #x-1fffffffffffe0000 #x-80000000
			 #x-3fffffffffffc0000 #x-40000000
			 #x-1fffffffffffe000000 #x-800000
			 #x-3fffffffffffc000000 #x-400000
			 #x-7fffffffffff8000000 #x-200000
			 #x-ffffffffffff0000000 #x-100000
			 #x-1fffffffffffe0000000 #x-80000
			 #x-3fffffffffffc0000000 #x-40000
			 #x-7fffffffffff80000000 #x-20000
			 #x-ffffffffffff00000000 #x-10000
			 #x-1fffffffffffe00000000 #x-8000
			 #x-3fffffffffffc00000000 
			 #x-4000 #x-3fffffffffffc000000000000 #x-1)))

#|| ;; This is the original form which won't compile under eclipse
(defparameter ash-results
    '(#x1 #x1 #x2 #x0 #x4 #x0 #x8 #x0 #x10 #x0 #x1000 #x0 #x2000 #x0 #x4000 #x0
      #x8000 #x0 #x10000 #x0 #x20000 #x0 #x40000 #x0 #x2000000 #x0 #x4000000 #x0
      #x8000000 #x0 #x10000000 #x0 #x20000000 #x0 #x40000000 #x0 #x80000000 #x0 #x100000000
      #x0 #x200000000 #x0 #x400000000 #x0 #x4000000000000 #x0 #x-1 #x-1 #x-2 #x-1 #x-4
      #x-1 #x-8 #x-1 #x-10 #x-1 #x-1000 #x-1 #x-2000 #x-1 #x-4000 #x-1 #x-8000 #x-1
      #x-10000 #x-1 #x-20000 #x-1 #x-40000 #x-1 #x-2000000 #x-1 #x-4000000 #x-1 #x-8000000
      #x-1 #x-10000000 #x-1 #x-20000000 #x-1 #x-40000000 #x-1 #x-80000000 #x-1 #x-100000000
      #x-1 #x-200000000 #x-1 #x-400000000 #x-1 #x-4000000000000 #x-1 #x5 #x5 #xa #x2 #x14
      #x1 #x28 #x0 #x50 #x0 #x5000 #x0 #xa000 #x0 #x14000 #x0 #x28000 #x0 #x50000
      #x0 #xa0000 #x0 #x140000 #x0 #xa000000 #x0 #x14000000 #x0 #x28000000 #x0 #x50000000
      #x0 #xa0000000 #x0 #x140000000 #x0 #x280000000 #x0 #x500000000 #x0 #xa00000000 #x0
      #x1400000000 #x0 #x14000000000000 #x0 #x-5 #x-5 #x-a #x-3 #x-14 #x-2 #x-28 #x-1
      #x-50 #x-1 #x-5000 #x-1 #x-a000 #x-1 #x-14000 #x-1 #x-28000 #x-1 #x-50000 #x-1
      #x-a0000 #x-1 #x-140000 #x-1 #x-a000000 #x-1 #x-14000000 #x-1 #x-28000000 #x-1
      #x-50000000 #x-1 #x-a0000000 #x-1 #x-140000000 #x-1 #x-280000000 #x-1 #x-500000000 #x-1
      #x-a00000000 #x-1 #x-1400000000 #x-1 #x-14000000000000 #x-1 #x9 #x9 #x12 #x4 #x24
      #x2 #x48 #x1 #x90 #x0 #x9000 #x0 #x12000 #x0 #x24000 #x0 #x48000 #x0 #x90000
      #x0 #x120000 #x0 #x240000 #x0 #x12000000 #x0 #x24000000 #x0 #x48000000 #x0
      #x90000000 #x0 #x120000000 #x0 #x240000000 #x0 #x480000000 #x0 #x900000000 #x0
      #x1200000000 #x0 #x2400000000 #x0 #x24000000000000 #x0 #x-9 #x-9 #x-12 #x-5 #x-24
      #x-3 #x-48 #x-2 #x-90 #x-1 #x-9000 #x-1 #x-12000 #x-1 #x-24000 #x-1 #x-48000 #x-1
      #x-90000 #x-1 #x-120000 #x-1 #x-240000 #x-1 #x-12000000 #x-1 #x-24000000 #x-1
      #x-48000000 #x-1 #x-90000000 #x-1 #x-120000000 #x-1 #x-240000000 #x-1 #x-480000000 #x-1
      #x-900000000 #x-1 #x-1200000000 #x-1 #x-2400000000 #x-1 #x-24000000000000 #x-1 #x7fff
      #x7fff #xfffe #x3fff #x1fffc #x1fff #x3fff8 #xfff #x7fff0 #x7ff #x7fff000 #x7
      #xfffe000 #x3 #x1fffc000 #x1 #x3fff8000 #x0 #x7fff0000 #x0 #xfffe0000 #x0 #x1fffc0000
      #x0 #xfffe000000 #x0 #x1fffc000000 #x0 #x3fff8000000 #x0 #x7fff0000000 #x0
      #xfffe0000000 #x0 #x1fffc0000000 #x0 #x3fff80000000 #x0 #x7fff00000000 #x0 #xfffe00000000
      #x0 #x1fffc00000000 #x0 #x1fffc000000000000 #x0 #x-7fff #x-7fff #x-fffe #x-4000
      #x-1fffc #x-2000 #x-3fff8 #x-1000 #x-7fff0 #x-800 #x-7fff000 #x-8 #x-fffe000 #x-4
      #x-1fffc000 #x-2 #x-3fff8000 #x-1 #x-7fff0000 #x-1 #x-fffe0000 #x-1 #x-1fffc0000 #x-1
      #x-fffe000000 #x-1 #x-1fffc000000 #x-1 #x-3fff8000000 #x-1 #x-7fff0000000 #x-1
      #x-fffe0000000 #x-1 #x-1fffc0000000 #x-1 #x-3fff80000000 #x-1 #x-7fff00000000 #x-1
      #x-fffe00000000 #x-1 #x-1fffc00000000 #x-1 #x-1fffc000000000000 #x-1 #xffff #xffff
      #x1fffe #x7fff #x3fffc #x3fff #x7fff8 #x1fff #xffff0 #xfff #xffff000 #xf #x1fffe000
      #x7 #x3fffc000 #x3 #x7fff8000 #x1 #xffff0000 #x0 #x1fffe0000 #x0 #x3fffc0000 #x0
      #x1fffe000000 #x0 #x3fffc000000 #x0 #x7fff8000000 #x0 #xffff0000000 #x0 #x1fffe0000000
      #x0 #x3fffc0000000 #x0 #x7fff80000000 #x0 #xffff00000000 #x0 #x1fffe00000000 #x0
      #x3fffc00000000 #x0 #x3fffc000000000000 #x0 #x-ffff #x-ffff #x-1fffe #x-8000 #x-3fffc
      #x-4000 #x-7fff8 #x-2000 #x-ffff0 #x-1000 #x-ffff000 #x-10 #x-1fffe000 #x-8 #x-3fffc000
      #x-4 #x-7fff8000 #x-2 #x-ffff0000 #x-1 #x-1fffe0000 #x-1 #x-3fffc0000 #x-1
      #x-1fffe000000 #x-1 #x-3fffc000000 #x-1 #x-7fff8000000 #x-1 #x-ffff0000000 #x-1
      #x-1fffe0000000 #x-1 #x-3fffc0000000 #x-1 #x-7fff80000000 #x-1 #x-ffff00000000 #x-1
      #x-1fffe00000000 #x-1 #x-3fffc00000000 #x-1 #x-3fffc000000000000 #x-1 #x1ffff #x1ffff
      #x3fffe #xffff #x7fffc #x7fff #xffff8 #x3fff #x1ffff0 #x1fff #x1ffff000 #x1f
      #x3fffe000 #xf #x7fffc000 #x7 #xffff8000 #x3 #x1ffff0000 #x1 #x3fffe0000 #x0
      #x7fffc0000 #x0 #x3fffe000000 #x0 #x7fffc000000 #x0 #xffff8000000 #x0 #x1ffff0000000
      #x0 #x3fffe0000000 #x0 #x7fffc0000000 #x0 #xffff80000000 #x0 #x1ffff00000000 #x0
      #x3fffe00000000 #x0 #x7fffc00000000 #x0 #x7fffc000000000000 #x0 #x-1ffff #x-1ffff
      #x-3fffe #x-10000 #x-7fffc #x-8000 #x-ffff8 #x-4000 #x-1ffff0 #x-2000 #x-1ffff000 #x-20
      #x-3fffe000 #x-10 #x-7fffc000 #x-8 #x-ffff8000 #x-4 #x-1ffff0000 #x-2 #x-3fffe0000 #x-1
      #x-7fffc0000 #x-1 #x-3fffe000000 #x-1 #x-7fffc000000 #x-1 #x-ffff8000000 #x-1
      #x-1ffff0000000 #x-1 #x-3fffe0000000 #x-1 #x-7fffc0000000 #x-1 #x-ffff80000000 #x-1
      #x-1ffff00000000 #x-1 #x-3fffe00000000 #x-1 #x-7fffc00000000 #x-1 #x-7fffc000000000000 #x-1
      #xfffff #xfffff #x1ffffe #x7ffff #x3ffffc #x3ffff #x7ffff8 #x1ffff #xfffff0 #xffff
      #xfffff000 #xff #x1ffffe000 #x7f #x3ffffc000 #x3f #x7ffff8000 #x1f #xfffff0000 #xf
      #x1ffffe0000 #x7 #x3ffffc0000 #x3 #x1ffffe000000 #x0 #x3ffffc000000 #x0 #x7ffff8000000
      #x0 #xfffff0000000 #x0 #x1ffffe0000000 #x0 #x3ffffc0000000 #x0 #x7ffff80000000 #x0
      #xfffff00000000 #x0 #x1ffffe00000000 #x0 #x3ffffc00000000 #x0 #x3ffffc000000000000 #x0
      #x-fffff #x-fffff #x-1ffffe #x-80000 #x-3ffffc #x-40000 #x-7ffff8 #x-20000 #x-fffff0
      #x-10000 #x-fffff000 #x-100 #x-1ffffe000 #x-80 #x-3ffffc000 #x-40 #x-7ffff8000 #x-20
      #x-fffff0000 #x-10 #x-1ffffe0000 #x-8 #x-3ffffc0000 #x-4 #x-1ffffe000000 #x-1
      #x-3ffffc000000 #x-1 #x-7ffff8000000 #x-1 #x-fffff0000000 #x-1 #x-1ffffe0000000 #x-1
      #x-3ffffc0000000 #x-1 #x-7ffff80000000 #x-1 #x-fffff00000000 #x-1 #x-1ffffe00000000 #x-1
      #x-3ffffc00000000 #x-1 #x-3ffffc000000000000 #x-1 #xf0001 #xf0001 #x1e0002 #x78000
      #x3c0004 #x3c000 #x780008 #x1e000 #xf00010 #xf000 #xf0001000 #xf0 #x1e0002000 #x78
      #x3c0004000 #x3c #x780008000 #x1e #xf00010000 #xf #x1e00020000 #x7 #x3c00040000 #x3
      #x1e0002000000 #x0 #x3c0004000000 #x0 #x780008000000 #x0 #xf00010000000 #x0
      #x1e00020000000 #x0 #x3c00040000000 #x0 #x7800080000000 #x0 #xf000100000000 #x0
      #x1e000200000000 #x0 #x3c000400000000 #x0 #x3c0004000000000000 #x0 #x-f0001 #x-f0001
      #x-1e0002 #x-78001 #x-3c0004 #x-3c001 #x-780008 #x-1e001 #x-f00010 #x-f001 #x-f0001000
      #x-f1 #x-1e0002000 #x-79 #x-3c0004000 #x-3d #x-780008000 #x-1f #x-f00010000 #x-10
      #x-1e00020000 #x-8 #x-3c00040000 #x-4 #x-1e0002000000 #x-1 #x-3c0004000000 #x-1
      #x-780008000000 #x-1 #x-f00010000000 #x-1 #x-1e00020000000 #x-1 #x-3c00040000000 #x-1
      #x-7800080000000 #x-1 #x-f000100000000 #x-1 #x-1e000200000000 #x-1 #x-3c000400000000 #x-1
      #x-3c0004000000000000 #x-1 #x7fffffff #x7fffffff #xfffffffe #x3fffffff #x1fffffffc
      #x1fffffff #x3fffffff8 #xfffffff #x7fffffff0 #x7ffffff #x7fffffff000 #x7ffff #xfffffffe000
      #x3ffff #x1fffffffc000 #x1ffff #x3fffffff8000 #xffff #x7fffffff0000 #x7fff #xfffffffe0000
      #x3fff #x1fffffffc0000 #x1fff #xfffffffe000000 #x3f #x1fffffffc000000 #x1f
      #x3fffffff8000000 #xf #x7fffffff0000000 #x7 #xfffffffe0000000 #x3 #x1fffffffc0000000 #x1
      #x3fffffff80000000 #x0 #x7fffffff00000000 #x0 #xfffffffe00000000 #x0 #x1fffffffc00000000
      #x0 #x1fffffffc000000000000 #x0 #x-7fffffff #x-7fffffff #x-fffffffe #x-40000000
      #x-1fffffffc #x-20000000 #x-3fffffff8 #x-10000000 #x-7fffffff0 #x-8000000 #x-7fffffff000
      #x-80000 #x-fffffffe000 #x-40000 #x-1fffffffc000 #x-20000 #x-3fffffff8000 #x-10000
      #x-7fffffff0000 #x-8000 #x-fffffffe0000 #x-4000 #x-1fffffffc0000 #x-2000 #x-fffffffe000000
      #x-40 #x-1fffffffc000000 #x-20 #x-3fffffff8000000 #x-10 #x-7fffffff0000000 #x-8
      #x-fffffffe0000000 #x-4 #x-1fffffffc0000000 #x-2 #x-3fffffff80000000 #x-1 #x-7fffffff00000000
      #x-1 #x-fffffffe00000000 #x-1 #x-1fffffffc00000000 #x-1 #x-1fffffffc000000000000 #x-1
      #xffffffff #xffffffff #x1fffffffe #x7fffffff #x3fffffffc #x3fffffff #x7fffffff8 #x1fffffff
      #xffffffff0 #xfffffff #xffffffff000 #xfffff #x1fffffffe000 #x7ffff #x3fffffffc000 #x3ffff
      #x7fffffff8000 #x1ffff #xffffffff0000 #xffff #x1fffffffe0000 #x7fff #x3fffffffc0000 #x3fff
      #x1fffffffe000000 #x7f #x3fffffffc000000 #x3f #x7fffffff8000000 #x1f #xffffffff0000000 #xf
      #x1fffffffe0000000 #x7 #x3fffffffc0000000 #x3 #x7fffffff80000000 #x1 #xffffffff00000000 #x0
      #x1fffffffe00000000 #x0 #x3fffffffc00000000 #x0 #x3fffffffc000000000000 #x0 #x-ffffffff
      #x-ffffffff #x-1fffffffe #x-80000000 #x-3fffffffc #x-40000000 #x-7fffffff8 #x-20000000
      #x-ffffffff0 #x-10000000 #x-ffffffff000 #x-100000 #x-1fffffffe000 #x-80000 #x-3fffffffc000
      #x-40000 #x-7fffffff8000 #x-20000 #x-ffffffff0000 #x-10000 #x-1fffffffe0000 #x-8000
      #x-3fffffffc0000 #x-4000 #x-1fffffffe000000 #x-80 #x-3fffffffc000000 #x-40 #x-7fffffff8000000
      #x-20 #x-ffffffff0000000 #x-10 #x-1fffffffe0000000 #x-8 #x-3fffffffc0000000 #x-4
      #x-7fffffff80000000 #x-2 #x-ffffffff00000000 #x-1 #x-1fffffffe00000000 #x-1
      #x-3fffffffc00000000 #x-1 #x-3fffffffc000000000000 #x-1 #xffffffffffff #xffffffffffff
      #x1fffffffffffe #x7fffffffffff #x3fffffffffffc #x3fffffffffff #x7fffffffffff8 #x1fffffffffff
      #xffffffffffff0 #xfffffffffff #xffffffffffff000 #xfffffffff #x1fffffffffffe000 #x7ffffffff
      #x3fffffffffffc000 #x3ffffffff #x7fffffffffff8000 #x1ffffffff #xffffffffffff0000 #xffffffff
      #x1fffffffffffe0000 #x7fffffff #x3fffffffffffc0000 #x3fffffff #x1fffffffffffe000000 #x7fffff
      #x3fffffffffffc000000 #x3fffff #x7fffffffffff8000000 #x1fffff #xffffffffffff0000000 #xfffff
      #x1fffffffffffe0000000 #x7ffff #x3fffffffffffc0000000 #x3ffff #x7fffffffffff80000000 #x1ffff
      #xffffffffffff00000000 #xffff #x1fffffffffffe00000000 #x7fff #x3fffffffffffc00000000 #x3fff
      #x3fffffffffffc000000000000 #x0 #x-ffffffffffff #x-ffffffffffff #x-1fffffffffffe
      #x-800000000000 #x-3fffffffffffc #x-400000000000 #x-7fffffffffff8 #x-200000000000
      #x-ffffffffffff0 #x-100000000000 #x-ffffffffffff000 #x-1000000000 #x-1fffffffffffe000
      #x-800000000 #x-3fffffffffffc000 #x-400000000 #x-7fffffffffff8000 #x-200000000
      #x-ffffffffffff0000 #x-100000000 #x-1fffffffffffe0000 #x-80000000 #x-3fffffffffffc0000
      #x-40000000 #x-1fffffffffffe000000 #x-800000 #x-3fffffffffffc000000 #x-400000
      #x-7fffffffffff8000000 #x-200000 #x-ffffffffffff0000000 #x-100000 #x-1fffffffffffe0000000
      #x-80000 #x-3fffffffffffc0000000 #x-40000 #x-7fffffffffff80000000 #x-20000
      #x-ffffffffffff00000000 #x-10000 #x-1fffffffffffe00000000 #x-8000 #x-3fffffffffffc00000000
      #x-4000 #x-3fffffffffffc000000000000 #x-1))
||#

(deftest ash
    (let ((results ash-results))
      (block test
	(dolist (n '(1 5 9 #x7fff #xffff #x1ffff #xfffff #xf0001 #x7fffffff #xffffffff #xffffffffffff))
	  (dolist (n (list n (- n)))
	    (dolist (off '(0 1 2 3 4 12 13 14 15 16 17 18 25 26 27 28 29 30 31 32 33 34 50))
	      (dolist (off (list off (- off)))
		(let ((got (ash n off))
		      (expected (pop results)))
		  (unless (= got expected)
		    (return-from test (values `(ash ,n ,off) got 'instead-of expected))))))))))
  nil)
