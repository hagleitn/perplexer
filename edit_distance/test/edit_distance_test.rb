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
    assert_equal(5,r[:cost])
    l = "abcdefghijkl"
    t = "bcdefghxlm"
    r = @ed.edit_distance_wrapper(l,t)
    assert_equal(7,r[:cost])
    l = "abrackadbrea"
    t = "abracadabra"
    r = @ed.edit_distance_wrapper(l,t)
    assert_equal(5,r[:cost])
  end
end
