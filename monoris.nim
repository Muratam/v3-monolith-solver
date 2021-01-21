import sequtils, heapqueue, hashes, random, strformat, sets
const width = 22
const height = 11
# 盤面: 0,[1,2,3,4]. 消すと周り4方向も消える
type Board = array[width, array[height, uint8]]
proc `$`(this: Board) : string =
  for y in 0..<height:
    for x in 0..<width:
      result &= fmt"{this[x][y]}"
    result &= "\n"

type Status = ref object
  board : Board
  hash : Hash
  deletedCount : int
  score : int
  counts : array[5, int]
func jHash(a:uint8, x, y: int) : Hash =
  const hash1000 = (proc(): array[1000, Hash] =
    for i in 0..<1000:
      result[i] = !$(i.hash)
    )()
  return hash1000[a] !& hash1000[5+x+y*width]
func recalcHash(s:Status): Hash =
  result = 0u8.hash
  for x in 0..<width:
    for y in 0..<height:
      result = result xor jHash(s.board[x][y], x, y)
func `$`(this:Status): string =
  result = fmt"hash: {this.hash}, delete: {this.deletedCount}, score: {this.score}\n counts: {this.counts} \n"
# 消した数が多い方を優先
func `<`(a,b : Status) : bool = a.score < b.score
proc solve(baseBoard: Board) =
  var pq = initHeapQueue[Status]()
  var used = initHashSet[Hash]()
  block:
    var s = new Status
    s.board = baseBoard
    s.deletedCount = 0
    s.hash = s.recalcHash()
    for x in 0..<width:
      for y in 0..<height:
        s.counts[s.board[x][y]] += 1
    s.counts[0] = 20000
    s.score = s.counts.min.int
    pq.push(s)
    used.incl s.hash
  var maxDeletedCount = -1
  while pq.len > 0:
    let s = pq.pop()
    if s.deletedCount > maxDeletedCount:
      maxDeletedCount = s.deletedCount
      echo s, " ", pq.len, " ",  maxDeletedCount
      echo s.board
    # 消せる場所を探して、ハッシュが存在していなければ登録
    var searched : array[width, array[height, bool]]
    for x in 0..<width:
      for y in 0..<height:
        let b = s.board[x][y]
        if b == 0 : continue
        if searched[x][y] : continue
        # 上・左はまず前提として異なるので調べなくて良い
        # if x > 0 and s.board[x-1][y] == b : continue
        # if y > 0 and s.board[x][y-1] == b : continue
        let downIsSame = y < height - 1 and s.board[x][y+1] == b
        let rightIsSame = x < width - 1 and s.board[x+1][y] == b
        if not(downIsSame or rightIsSame): continue
        # つながっている場所を全部消す
        var newHash = s.hash
        var deletes = newSeq[tuple[x,y:int]]()
        proc search(x,y:int) =
          searched[x][y] = true
          deletes &= (x,y)
          newHash = newHash xor jHash(b, x, y) xor jHash(0, x, y)
          for (nx, ny) in [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]:
            if nx < 0 or nx >= width: continue
            if ny < 0 or ny >= height: continue
            if searched[nx][ny] : continue
            if b != s.board[nx][ny] : continue
            search(nx,ny)
        search(x, y)
        if newHash in used : continue
        var newS = new Status
        newS.board = s.board
        newS.deletedCount = s.deletedCount + deletes.len
        newS.hash = newHash
        newS.counts = s.counts
        # 周辺の値は+1する
        for (sx, sy) in deletes:
          for (nx, ny) in [(sx-1,sy), (sx+1,sy), (sx,sy-1), (sx,sy+1)]:
            if nx < 0 or nx >= width: continue
            if ny < 0 or ny >= height: continue
            if newS.board[nx][ny] == 0 : continue
            if newS.board[nx][ny] >= 100 : continue
            newS.board[nx][ny] += 100u8 + 1u8
        for (sx, sy) in deletes:
          for (nx, ny) in [(sx-1,sy), (sx+1,sy), (sx,sy-1), (sx,sy+1)]:
            if nx < 0 or nx >= width: continue
            if ny < 0 or ny >= height: continue
            if newS.board[nx][ny] < 100 : continue
            newS.board[nx][ny] -= 100u8
            if newS.board[nx][ny] == 5 :
              newS.board[nx][ny] = 1
              newS.counts[4] -= 1
            else:
              newS.counts[newS.board[nx][ny] - 1] -= 1
            newS.counts[newS.board[nx][ny]] += 1
        for (sx, sy) in deletes:
          let b = newS.board[sx][sy]
          newS.counts[b] -= 1
          newS.counts[0] += 1
          newS.board[sx][sy] = 0
        newS.score = newS.counts.min
        pq.push(newS)
        used.incl newHash


block:
  var baseBoard: Board
  randomize()
  for x in 0..<width:
    for y in 0..<height:
      baseBoard[x][y] = rand(1..4).uint8
  baseBoard.solve()
