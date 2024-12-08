0:
	ghc j0/test.hs
	j0/test < j0/input.txt

1:
	ghc j1/main.hs -o j1/main.exe
	j1/main.exe

2:
	ghc j2/main.hs -o j2/main.exe
	j2/main.exe

3:
	ghc j3/main.hs -o j3/main.exe
	j3/main.exe

4:
	ghc j4/main.hs -o j4/main.exe
	j4/main.exe

5:
	ghc j5/main.hs -package=containers-0.7 -o j5/main.exe
	j5/main.exe

6:
	ghc -O2 j6/main.hs -o j6/main.exe
	time j6/main.exe

7:
	ghc -O2 j7/main.hs -o j7/main.exe
	time j7/main.exe