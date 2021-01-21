import sequtils, heapqueue, hashes, random, strformat, sets, algorithm
import nimPNG
const width = 22
const height = 11
const maxN = 4
# 盤面: 0,[1,2,3,4]. 消すと周り4方向も消える
type Board = array[width, array[height, uint8]]
proc `$`(this: Board) : string =
  const toZenkaku = ["　", "１", "２", "３", "４", "５", "６"]
  for y in 0..<height:
    for x in 0..<width:
      result &= fmt"{toZenkaku[this[x][y]]}"
    result &= "\n"

type Status = ref object
  board : Board
  hash : Hash
  deletedCount : int
  deletedCountForScore : int
  score : int
  counts : array[maxN+1, int]
  preStatus : Status
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
  result = fmt"hash: {this.hash}, left: {width * height - this.deletedCount}, score: {this.score}\n counts: {this.counts} \n"
func printAll(this:Status): string =
  var statuses = newSeq[Status]()
  block:
    var status = this
    while status.preStatus != nil:
      statuses &= status
      status = status.preStatus
    statuses &= status
    statuses.reverse()
  result = ""
  for i,status in statuses:
    result &= fmt "################ {i} ################\n"
    const toZenkaku = ["　", "１", "２", "３", "４", "５", "６"]
    for y in 0..<height:
      for x in 0..<width:
        if i > 0 and status.board[x][y] == 0 and
            statuses[i-1].board[x][y] != 0:
          result &= "＠"
        else:
          result &= fmt"{toZenkaku[status.board[x][y]]}"
      result &= "\n"
    result &= "\n"
# 消した数が多い方を優先
func `<`(a,b : Status) : bool = a.score > b.score
func getMaxSquareSize(s:Status) : uint8 =
  var memo: array[width, array[height, uint8]]
  memo[0][0] = if s.board[0][0] != 0 : 0 else: 1
  for x in 1..<width:
    memo[x][0] = if s.board[x][0] != 0 : 0u8 else: 1u8
  for y in 1..<height:
    memo[0][y] = if s.board[0][y] != 0 : 0u8 else: 1u8
  result = 1
  for x in 1..<width:
    for y in 1..<height:
      if s.board[x][y] != 0 :
        memo[x][y] = 0
        continue
      memo[x][y] = 1u8 + min(memo[x-1][y-1], min(memo[x-1][y], memo[x][y-1]))
      result = result.max(memo[x][y])
proc calcScore(s: Status, weight:int) : int =
  var minVal = 10000
  var maxVal = 0
  for i in 1..maxN:
    minVal = minVal.min s.counts[i]
    maxVal = maxVal.max s.counts[i]
  let sq = 0 #rand(0..1) + s.getMaxSquareSize().int
  return - minVal * weight + maxVal + s.deletedCountForScore + sq

var maxDeletedCount = -1
proc solve(baseBoard: Board, weight: int ) =
  echo "weight:", weight
  var pq = initHeapQueue[Status]()
  var used = initHashSet[Hash]()
  block:
    var s = new Status
    s.board = baseBoard
    s.deletedCount = 0
    s.deletedCountForScore = 0
    s.hash = s.recalcHash()
    for x in 0..<width:
      for y in 0..<height:
        s.counts[s.board[x][y]] += 1
    s.counts[0] = 20000
    s.score = s.calcScore(weight)
    s.preStatus = nil
    pq.push(s)
    used.incl s.hash
  while pq.len > 0 and pq.len < 1000000:
    let s = pq.pop()
    if s.deletedCount > maxDeletedCount:
      maxDeletedCount = s.deletedCount
      echo s, " ", pq.len, " ",  maxDeletedCount
      echo s.board
      echo weight
      var f = open("result.txt", fmWrite)
      defer: f.close()
      f.write s.printAll()
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
        newS.deletedCount = s.deletedCount
        newS.deletedCountForScore = s.deletedCountForScore
        newS.hash = newHash
        newS.counts = s.counts
        newS.preStatus = s
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
            if newS.board[nx][ny] == maxN + 1 :
              newS.board[nx][ny] = 1
              newS.counts[maxN] -= 1
            else:
              newS.counts[newS.board[nx][ny] - 1] -= 1
            newS.counts[newS.board[nx][ny]] += 1
        for (sx, sy) in deletes:
          let b = newS.board[sx][sy]
          newS.counts[b] -= 1
          newS.counts[0] += 1
          newS.board[sx][sy] = 0
          newS.deletedCount += 1
          # if sy < height - 1 and sx < width - 2 :
          newS.deletedCountForScore += 1
        newS.score = newS.calcScore(weight)
        pq.push(newS)
        used.incl newHash

proc parseToBoard(pngPath:string) : Board =
  let png = loadPNG24(pngPath)
  const format = 3
  let size = 75 # int(png.height / (height + 3))
  var colorBoard : array[width, array[height, string]]
  const xOffset = 2
  const yOffset = 1
  for vx in xOffset..<xOffset+width:
    for vy in yOffset..<yOffset+height:
      let bx = vx * size + size div 2
      let by = vy * size + size div 2
      let rgb = png.data[format*(bx+by*png.width)..<format*(bx+by*png.width+1)]
      let x = vx - xOffset
      let y = vy - yOffset
      colorBoard[x][y] = rgb
  block:
    var boardEncoded = ""
    const rate = 10
    for y in 0..<height * rate:
      for x in 0..<width * rate:
        let rgb = colorBoard[x div rate][y div rate]
        boardEncoded &= rgb
    var ok = savePNG24("monorisout.png",boardEncoded,width*rate,height*rate)
    for y in 0..<height:
      for x in 0..<width:
        let rgb = colorBoard[x][y]
        let ru = rgb[0].uint8
        let gu = rgb[1].uint8
        let bu = rgb[2].uint8
        let u = ru.int + gu.int + bu.int
        let r = ru.int / u
        let g = gu.int / u
        let b = bu.int / u
        if b < 0.27 : result[x][y] = 3 # Y
        elif r > 0.37 : result[x][y] = 2 # R
        elif r < 0.27 : result[x][y] = 4 # B
        else: result[x][y] = 1
        echo fmt"{r:.2} {g:.2} {b:.2} :: {result[x][y]}"

block:
  var baseBoard: Board
  if true:
    baseBoard = parseToBoard("monoris.png")
  if false:
    randomize()
    for x in 0..<width:
      for y in 0..<height:
        baseBoard[x][y] = rand(1..maxN).uint8
  for weight in 1..20:
    baseBoard.solve(rand(1..30))
