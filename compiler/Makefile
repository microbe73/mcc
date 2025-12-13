mcc.amd64-darwin: $(wildcard *.sml)
	printf 'CM.make "lang.cm";\nSMLofNJ.exportML("mcc");\n' | sml

test.s: mcc.amd64-darwin test.c
	printf 'Main.compile("test.c","test.s");\n' | sml @SMLload $<
	sed -i '' 's/~/-/g' $@

test: test.s
	arch -x86_64 clang $< -o $@

clean:
	rm -f test
