#!/usr/bin/perl


my $care=0;
my $header="";

while (<>) {
	chomp();
	if (m/^####[^%]*[A-Z]*[^%]*[a-z]+[^%]*%%%%/) {
		s/####[^%]*%%%%.?see [^#]*//g;
		s/#### //;
		s/ *%%%%//;
		$header=$_;
		#print("HEADER\t$_\n");
		$care = 0;
	} elsif (m/^\? /) {
		$care = 0 ;
	} elsif (( $care ) || (m/^l .*/) || (m/^.* l ####/)) {
		$care = 1;
		my $line = $_;
		if ($line =~ m/####/) {
			$line =~ s/.*####([^%]*)%%%%[^#]*/$header\t\1\n/g;
			print( $line );
		}
	}
}

