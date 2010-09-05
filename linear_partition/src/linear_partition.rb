# Given a set of non-negative integers {a1,a2,..,an} and a
# non-negative number k, partition S in k ranges without changing the
# order of the numbers so as to minimize the maximum sum of the
# ranges

#l,k are 0 indexed
def linear_partition(l,k,w,mtable)
  return nil if w == 0 && k == 1
  return mtable[w-1][k-1] = (0..w-1).inject(0){|s,x| s+= l[x]} if k == 1
  return mtable[w-1][k-1] if mtable[w-1][k-1] != nil 
  min = 0
  (w-1).downto(0) { |j|
    s1 = linear_partition(l,k-1,j,mtable)
    s2 = (j..w-1).inject(0){|s,x| s+= l[x]}
    s1 = s2 if s1.nil? || s2 > s1
    min = s1 if (min > s1 || j == (w-1) )
    mtable[w-1][k-1] = min
  } 
  return min
end

def backtrack_partition(l,r,c,b,arr)
  return [] if c < 0 || r < 0
  part = nil
  if !arr[r-1][c-1].nil? && (arr[b][c] >= arr[r-1][c-1] || c == 0)
    part = (r..b).map{|x| l[x] }#jump ship
    b,c = r-1,c-1
  end
  return (backtrack_partition(l,r-1,c,b,arr)<< part).compact
end

def linear_partition_test(k,n,arr)
  mtable = Array.new(n) {Array.new(k,nil)}
  linear_partition(arr,k,n,mtable)
  mtable.each {|x| p x}
  partitions = backtrack_partition(arr,n-1,k-1,n-1,mtable)
  p partitions
end

if ARGV.size < 2
  p "usage: linear_partition.rb <csv for input set> <partitions>"
  exit
end
arr = ARGV[0].split(',').map{ |x| x.to_i}
k = ARGV[1].to_i
linear_partition_test(k,arr.size,arr)
