
FILE=~/Library/Application\ Support/Sibelius\ Software/Sibelius\ 6/Plugins/Test/Test.plg

.PHONY: test
test:
	dist/build/baritone/baritone --include-core <test/Foo.hs >test/Foo.plg;
	(echo '\xFF\xFE\c' & dist/build/baritone/baritone | iconv -f UTF-8 -t UTF-16LE) <test/Foo.hs >$(FILE);
	open -a /Applications/Sibelius*;

stop:
	killall "Sibelius 6";