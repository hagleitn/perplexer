#this is a driver class for the EditDistance class
require 'src/edit_distance'

p EditDistance.new(1,2,3).edit_distance_wrapper(ARGV[0], ARGV[1]);

