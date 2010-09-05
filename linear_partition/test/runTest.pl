use strict;
use warnings;

my $i = 0;

while(<>) {
  chomp;
  my ($array,$k,$result) = split / /;

  my $cmd = "echo \"result($array,$k,_,S).\" | gprolog --init-goal \"consult(linear)\"";
  my $res_prolog = `$cmd`;

  $res_prolog =~ m/^.*S = ([0-9]*).*$/m;
  $res_prolog = $1;
  chomp($res_prolog);

  $array =~ s/[\[\]]//g;
  my $res_ruby = `ruby ../src/ruby/linear_partition.rb $array $k`;
  chomp($res_ruby);

  if ($res_prolog eq $result) {
    print "Prolog: $res_prolog == $result\n";
  } else {
    print STDERR "Prolog: $array $k => $res_prolog (!= $result)\n";
  }

  if ($res_ruby eq $result) {
    print "Ruby: $res_ruby == $result\n";
  } else {
    print STDERR "Prolog: $array $k => $res_ruby (!= $result)\n";
  }

  $i++;
}

print "Done. $i partitions complete.\n";
