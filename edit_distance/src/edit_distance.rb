#Author: Vidit Drolia (viditdrolia@gmail.com)
#Modified: 09/06/2010
#Find the miniumum edit distance between two strings for a given set
#of allowed transformations (insert,delete,replace)

class EditDistance
  attr_accessor :replace_cost, :delete_cost, :insert_cost
  def initialize
    @replace_cost = 1
    @delete_cost = 2
    @insert_cost = 3
  end
  def edit_distance(l,t,i,x,mtable)
    return 0 if i == l.size && x == t.size
    return @delete_cost * (l.size - i) if x == t.size && i < l.size
    return @insert_cost * (t.size - x) if i == l.size && x < t.size
    if l[i] ==  t[x]
      return mtable[i][x] = edit_distance(l,t,i+1,x+1,mtable)
    else
      return mtable[i][x] if !mtable[i][x].nil?
      ci = edit_distance(l,t,i+1,x,mtable) 
      ci = ci.nil? ? @insert_cost : ci + @insert_cost
      cr = edit_distance(l,t,i+1,x+1,mtable)
      cr = cr.nil? ? @replace_cost : cr + @replace_cost
      cd = edit_distance(l,t,i+1,x,mtable)
      cd = cd.nil? ? @delete_cost : cd + @delete_cost
      min = [ci,cr,cd].min
      mtable[i][x] = min
      return min 
    end
  end
  def transform(op,l,t,i,j,table)
    j = t.size if j > t.size
    s = t[0,j] + l[i,l.size] 
  end

  #TODO clean this mess up
  #pseudocode: start form the first cell, keep jumping to the
  #adjacent cell with minimum cost. if you hit a wall, then you can
  #either go down or sideways. Matrix movement interpretation:
  #
  # * diagonal = replace
  # * down = delete
  # * side/right = insert

  def edit_distance_bt(l,t,table)
    res = [{:op => :init, :str => l}]
    i,j = 0,0,0,0
    while (i < l.size || j < t.size ) 
      op,min,c1,c2,c3 = :nop,0,nil,nil,nil
      if i == (l.size-1) && (j == t.size-1) && table[i][j] != 0
        op = :replace
        min,i2,j2 = 1,i+1,j+1 
      elsif j == t.size
        op,i2,j2 = :delete,i+1,j 
      elsif i == l.size 
        op,i2,j2 = :insert,i,j+1
      else
        c0 = table[i][j]
        c1 = table[i+1][j+1] if !table[i+1].nil? && !table[i+1][j+1].nil?  #replace
        c2 = table[i+1][j]  if !table[i+1].nil? && !table[i+1][j].nil? #delete
        c3 = table[i][j+1] if !table[i].nil? && !table[i][j+1].nil?  #insert
        op,min,i2,j2 = :replace,c0,i+1,j+1
        min,op,i2,j2 = c1,:replace,i+1,j+1 if !c1.nil? && c1 <= min && (c0 - c1 == 1)
        min,op,i2,j2 = c2,:delete,i+1,j if  !c2.nil? && c2 <= min && (c0 - c2 == 2)
        min,op,i2,j2 = c3,:insert,i,j+1 if !c3.nil? && c3 <= min && (c0 - c3 == 3)
        if min == c0
          if op == :replace || op == :delete || op == :insert
            op = :nop
          else
            p "fatal: cannot find a destination cell" if $DEBUG
            p ({:p => {:i => i, :j => j},:op => op, :min => min, :replace => c1, :delete => c2, :insert => c3}) if $DEBUG
          end
        end
      end
      p ({:p => {:i => i, :j => j},:op => op, :min => min, :replace => c1, :delete => c2, :insert => c3}) if $DEBUG
        i,j = i2,j2
      r = {:op => op, :str => transform(op,l,t,i,j,table)}
      res<< r if r[:op] != :nop
    end
    res
  end

  def edit_distance_wrapper(iarr,tarr)
    mtable = Array.new(iarr.size){Array.new(tarr.size,nil)}
    lp,tp = 0,0
    edist = edit_distance(iarr,tarr,lp,tp,mtable)
    mtable.each{|r| p r} if $DEBUG
    steps = edit_distance_bt(iarr,tarr,mtable)
    return {:cost => edist, :steps => steps}
  end
end

p EditDistance.new().edit_distance_wrapper(ARGV[0], ARGV[1]);
