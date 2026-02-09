# So sánh VoteParty Mode Cũ vs Mode Mới (RollingWindow24h)

## 📋 Khi nào VotedTimestamps được XÓA?

### ✅ XÓA Timestamps (reset với override=true):
1. **Khi VoteParty reset theo ngày** (`ResetEachDay: true`)
   - Event: `DayChangeEvent`
   - Gọi: `reset(true)`
   - Xóa: Total votes, VotedUsers list, **VotedTimestamps map**

2. **Khi VoteParty reset theo tuần** (`ResetWeekly: true`)
   - Event: `WeekChangeEvent`
   - Gọi: `reset(true)`
   - Xóa: Total votes, VotedUsers list, **VotedTimestamps map**

3. **Khi VoteParty reset theo tháng** (`ResetMonthly: true`)
   - Event: `MonthChangeEvent`
   - Gọi: `reset(true)`
   - Xóa: Total votes, VotedUsers list, **VotedTimestamps map**

### ❌ KHÔNG XÓA Timestamps (reset với override=false):
- **Khi VoteParty nổ** (sau khi give rewards)
  - Gọi: `reset(false)` từ `giveRewards()`
  - Giữ lại: **VotedTimestamps map** (để filter cho lần sau)
  - Xóa: VotedUsers list (nhưng không quan trọng với rolling window mode)

---

## 🔄 So sánh Mode Cũ vs Mode Mới

### 📊 MODE CŨ (`RollingWindow24h: false`)

#### Cách hoạt động:
1. **Khi player vote:**
   - Thêm vào `VoteParty.Voted` list (nếu chưa có)
   - **KHÔNG** lưu timestamp

2. **Khi VoteParty nổ:**
   - Lấy danh sách từ `VoteParty.Voted` list
   - Give reward cho tất cả players trong list
   - Reset: Clear `VoteParty.Voted` list

3. **Reset behavior:**
   - Sau mỗi VoteParty: Clear `VoteParty.Voted` list
   - Theo ngày/tuần/tháng: Clear tất cả (nếu config)

#### Vấn đề:
- ❌ Nếu player vote lần 1, VoteParty nổ lần 1 → player nhận reward
- ❌ Nếu VoteParty nổ lần 2, 3, 4 → player KHÔNG nhận reward (vì không có trong Voted list mới)
- ❌ Player phải vote lại mỗi lần để được nhận reward

---

### 🆕 MODE MỚI (`RollingWindow24h: true`)

#### Cách hoạt động:
1. **Khi player vote:**
   - Thêm vào `VoteParty.Voted` list (backward compatibility)
   - **LƯU timestamp** vào `VoteParty.VotedTimestamps.<UUID>`

2. **Khi VoteParty nổ:**
   - Lấy tất cả timestamps từ `VoteParty.VotedTimestamps` map
   - **Filter theo 24h rolling window**: Chỉ giữ players có timestamp >= (currentTime - 24h)
   - Give reward cho players hợp lệ
   - Reset: **GIỮ LẠI** timestamps map (chỉ clear Voted list)

3. **Reset behavior:**
   - Sau mỗi VoteParty: **GIỮ LẠI** timestamps (để filter cho lần sau)
   - Theo ngày/tuần/tháng: Clear tất cả (nếu config)

#### Ưu điểm:
- ✅ Player vote 1 lần → có thể nhận reward nhiều lần (nếu vote trong 24h)
- ✅ VoteParty nổ lần 2, 3, 4 → player vẫn nhận reward (nếu vote trong 24h)
- ✅ Tự động filter theo thời gian thực (rolling window)

---

## 📝 Bảng So Sánh Chi Tiết

| Tiêu chí | Mode Cũ | Mode Mới (RollingWindow24h) |
|----------|---------|----------------------------|
| **Lưu trữ dữ liệu** | `VoteParty.Voted` (list) | `VoteParty.VotedTimestamps` (map) |
| **Khi player vote** | Thêm vào Voted list | Thêm vào Voted list + Lưu timestamp |
| **Khi VoteParty nổ** | Give reward cho tất cả trong Voted list | Filter theo 24h window từ timestamps map |
| **Reset sau VoteParty** | Clear Voted list | Giữ timestamps, chỉ clear Voted list |
| **Reset theo ngày/tuần/tháng** | Clear tất cả | Clear tất cả (bao gồm timestamps) |
| **Player vote 1 lần** | Chỉ nhận reward lần 1 | Có thể nhận nhiều lần (trong 24h) |
| **Player không vote lại** | Không nhận reward lần sau | Vẫn nhận reward (nếu trong 24h) |
| **Filter theo thời gian** | ❌ Không | ✅ Có (24h rolling window) |

---

## 🔍 Code Flow Comparison

### Mode Cũ:
```
Player votes → addVotePlayer() → Add to Voted list
VoteParty triggers → giveRewards() → Get from Voted list → Give rewards → reset(false) → Clear Voted list
```

### Mode Mới:
```
Player votes → addVotePlayer() → Add to Voted list + Save timestamp
VoteParty triggers → giveRewards() → Get timestamps map → Filter 24h → Give rewards → reset(false) → Keep timestamps
```

---

## ⚠️ Lưu Ý Quan Trọng

1. **Timestamps chỉ được xóa khi:**
   - `ResetEachDay: true` → Xóa vào 00:00 mỗi ngày
   - `ResetWeekly: true` → Xóa vào đầu tuần
   - `ResetMonthly: true` → Xóa vào đầu tháng

2. **Timestamps KHÔNG được xóa khi:**
   - VoteParty nổ (reset với override=false)
   - Server restart (timestamps persist trong file)

3. **Vấn đề tiềm ẩn:**
   - Timestamps sẽ tích lũy theo thời gian (nếu không có reset theo ngày/tuần/tháng)
   - Players vote cũ hơn 24h sẽ không được filter vào (đúng behavior)
   - Cần cleanup định kỳ hoặc reset theo ngày/tuần/tháng

---

## 🎯 Kết Luận

**Mode mới (RollingWindow24h)** giải quyết vấn đề của mode cũ:
- ✅ Players không cần vote lại mỗi lần VoteParty nổ
- ✅ Tự động filter theo thời gian thực (24h rolling window)
- ✅ Dữ liệu persist qua server restart
- ✅ Backward compatible (vẫn giữ Voted list cho mode cũ)
