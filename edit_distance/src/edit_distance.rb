#Author: Vidit Drolia (viditdrolia@gmail.com)
#Find the miniumum edit distance between two strings for a given set
#of allowed transformations (insert,delete,replace)

class EditDistance
  attr_accessor :replace, :delete, :insert,:table
  def initialize(replace,delete,insert)
    @replace,@delete,@insert = replace,delete,insert
  end
  def edit_distance(l,t,i,x)
    return 0 if i == l.size && x == t.size
    return @delete * (l.size - i) if x == t.size && i < l.size
    return @insert * (t.size - x) if i == l.size && x < t.size
    if l[i] ==  t[x]
      return @table[i][x] = edit_distance(l,t,i+1,x+1)
    else
      return @table[i][x] if !@table[i][x].nil?
      ci = @insert + edit_distance(l,t,i+1,x) 
      cr = @replace + edit_distance(l,t,i+1,x+1)
      cd = @delete + edit_distance(l,t,i+1,x)
      return @table[i][x] = [ci,cr,cd].min
    end
  end
  def transform(op,l,t,i,j)
    j = t.size if j > t.size
    t[0,j] + l[i,l.size] 
  end

  #pseudocode: start form the first cell, keep jumping to the
  #adjacent cell with minimum cost. if you hit a wall, then you can
  #either go down or sideways. Matrix movement interpretation:
  #
  # * diagonal = replace
  # * down = delete
  # * side/right = insert

  def lookup_table(i,j)
    return @table[i][j] if !@table.nil? && !@table[i].nil? && !@table[i][j].nil?
  end
  def next_cell(op,i,j)
    return i+1,j+1 if op == :nop
    return i+1,j+1 if op == :replace
    return i,j+1 if op == :insert
    return i+1,j if op == :delete
  end
  def valid_min?(c0,cx,min,cost)
    return true if !cx.nil? && cx <= min && (c0-cx == cost)
  end
  def edit_distance_bt(l,t)
    res = [{:op => :init, :str => l}]
    i,j = 0,0,0,0
    while (i < l.size || j < t.size) 
      op,min,c1,c2,c3 = :nop,0,nil,nil,nil
      if i == (l.size-1) && (j == t.size-1) && @table[i][j] != 0
        min,op = @table[i][j],:replace
      elsif j == t.size
        op = :delete
      elsif i == l.size 
        op = :insert
      else
        c0 = @table[i][j]
        c1 = lookup_table(i+1,j+1)
        c2 = lookup_table(i+1,j)
        c3 = lookup_table(i,j+1)
        min,op = c0,:replace
        min,op = c1,:replace if valid_min?(c0,c1,min,1)
        min,op = c2,:delete if valid_min?(c0,c2,min,2)
        min,op = c3,:insert if valid_min?(c0,c3,min,3)
        op = :nop if min == c0
      end
      p ({:p => {:i => i, :j => j},:op => op, :min => min, :replace => c1, :delete => c2, :insert => c3}) if $DEBUG
      i,j = next_cell(op,i,j)
      res<< {:op => op, :str => transform(op,l,t,i,j)} if :op != :nop
    end
    res
  end

  def edit_distance_wrapper(iarr,tarr)
    @table = Array.new(iarr.size){Array.new(tarr.size,nil)}
    edist = edit_distance(iarr,tarr,0,0)
    @table.each{|r| p r} if $DEBUG
    steps = edit_distance_bt(iarr,tarr)
    return {:cost => edist, :steps => steps}
  end
end
