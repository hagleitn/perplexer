#Author: Vidit Drolia (viditdrolia@gmail.com)
#Modified: 09/06/2010
#Find the miniumum edit distance between two strings for a given set
#of allowed transformations (

@replace_cost = 1
@delete_cost = 2
@insert_cost = 3

def edit_distance(l,t,i,x,mtable)
  return nil if i == l.size && x == t.size
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
    p "ci = #{ci} cr = #{cr} cd = #{cd}" if $DEBUG
    min = [ci,cr,cd].min
    mtable[i][x] = min
    p "min = #{min}" if $DEBUG
    return min 
  end
end

def edit_distance_wrapper(iarr,tarr)
  mtable = Array.new(iarr.size){Array.new(tarr.size,nil)}
  lp,tp = 0,0
  edist = edit_distance(iarr,tarr,lp,tp,mtable)
  return edist,mtable
end

iarr = "abcdefghijkl"
tarr = "bcdeffghixkl"
m,tab = edit_distance_wrapper(iarr,tarr)
p "min = #{m}" id $DEBUG
tab.each {|x| s = ""; x.each { |i| s = s + i.inspect + "\t"}; puts s,"\n"} if $DEBUG
