require 'src/edit_distance'
require 'test/unit'

class EditDistanceTest < Test::Unit::TestCase
  def setup
    @ed = EditDistance.new(1,2,3)
  end
  def test_edit_distance
    l = "foo"
    t = "bar"
    r = @ed.edit_distance_wrapper(l,t)
    r[:steps]
    assert_equal(3,r[:cost])

    l = "foo1"
    t = "baz"
    r = @ed.edit_distance_wrapper(l,t)
    p r[:steps]
    assert_equal(5,r[:cost])

    l = "abcdefghijkl"
    t = "bcdefghxlm"
    r = @ed.edit_distance_wrapper(l,t)
    p r[:steps]
    assert_equal(7,r[:cost])

    l = "abrackadbrea"
    t = "abracadabra"
    p({:l => l, :t => t}) if $DEBUG
    r = @ed.edit_distance_wrapper(l,t)
    assert_equal(5,r[:cost])
    p r[:steps]
    l = "xtesting"
    t = "testings"
    p({:l => l, :t => t}) if $DEBUG
    r = @ed.edit_distance_wrapper(l,t)
    assert_equal(5,r[:cost])
    p r[:steps]

  end
end
