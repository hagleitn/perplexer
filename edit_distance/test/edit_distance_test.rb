require 'src/edit_distance'
require 'test/unit'

class EditDistanceTest < Test::Unit::TestCase
  def setup
    @ed = EditDistance.new
  end
  def test_edit_distance
    l = "foo1"
    t = "baz"
    r = @ed.edit_distance_wrapper(l,t)
    r[:steps].each {|x| p x} #if $DEBUG
    assert_equal(5,r[:cost])

    l = "abcdefghijkl"
    t = "bcdefghxlm"
    r = @ed.edit_distance_wrapper(l,t)
    r[:steps].each {|x| p x} #if $DEBUG
    assert_equal(7,r[:cost])

    l = "abrackadbrea"
    t = "abracadabra"
    p({:l => l, :t => t}) if $DEBUG
    r = @ed.edit_distance_wrapper(l,t)
    assert_equal(5,r[:cost])
    r[:steps].each {|x| p x} #if $DEBUG
  end
end
