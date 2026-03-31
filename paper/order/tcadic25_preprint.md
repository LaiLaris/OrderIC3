# tcadic25_preprint 阅读笔记

## 1. 论文主题

这篇论文对应的是：

- `Revisiting Assumptions Ordering in CAR-Based Model Checking`

它研究的问题非常聚焦：

- 在 `CAR` 里，SAT solver 的 `assumption literals` 排序方式，会直接影响 `unsat core (UC)` 的形状；
- `UC` 的形状又会影响 `CAR` 收紧 `O-frames` 的速度；
- 因而 assumption ordering 本身就是一个重要的性能优化点。

论文的主线不是改 CAR 的大框架，而是改：

- **当前 state cube 里的 literals 应该以什么顺序送进 SAT solver**。

作者从已有的 `Intersection + Rotation` 出发，进一步提出：

- `CoreLocality`
- `conflict-literal prioritization`
- `Hybrid-CAR`

这三部分构成了论文的主要贡献。

---

## 2. 论文的背景：为什么 assumption ordering 会重要

CAR 的 SAT 调用可以理解成：

```text
SAT(ϕ, A)
```

其中：

- `ϕ` 是 CNF 公式；
- `A` 是一串 assumption literals。

现代 CDCL SAT solver 处理 assumptions 的方式是：

- 按给定顺序把它们作为最开始的 decisions；
- 每加入一个 decision，就执行 BCP；
- 如果在 assumptions 这些决策层内就发生冲突，就返回 `UNSAT`；
- 再从 assumptions 中提取一个 `UC`。

因此：

- assumptions 的顺序不是无关紧要的；
- 排在前面的 literal，更可能进入传播链和冲突分析；
- 也更可能出现在最终的 `UC` 里。

这就是论文的出发点：

- **改变 literal 顺序，就可能改变 UC；**
- **改变 UC，就可能改变 CAR 的搜索效率。**

---

## 3. CAR 在这篇论文里的角色

论文关注的是 `CAR` 的 bug-finding 性能。

作者的基本观点是：

- CAR 维护一串 `O-frames`；
- 每次 `UNSAT` 得到的 `UC` 都会用来收紧这些 over-approximating frames；
- 如果 `UC` 更有利，就能更快地删掉不必要状态；
- 于是证明“当前 state 到不了某个 O-frame”这件事会更快。

所以这篇论文优化的是：

- `UNSAT proof` 的局部结构；
- `UC` 的质量；
- 进而优化 `CAR` 的 proof-finding / bug-finding 效率。

---

## 4. 旧方法一：Intersection

### 4.1 基本做法

`Intersection` 用到的信息是：

- 当前 state `s`
- 上一次 `UNSAT` 得到的 `last UC`

做法是：

- 取 `s ∩ lastUC`；
- 把这部分 literals 放到 assumptions 的最前面；
- 其他 literals 再接到后面。

也就是说：

```text
priority = s ∩ lastUC
```

### 4.2 直觉

它的核心假设是：

- 如果上一轮 proof 依赖某些 literal；
- 那么下一轮 proof 很可能还依赖相近的 literal。

因此：

- 把这些刚刚出现在 `UC` 中的 literal 提前；
- 新得到的 `UC` 更可能和旧 `UC` 相似；
- 连续 proof 会更“局部化”。

### 4.3 论文里的解释：core locality

这篇论文对 `Intersection` 的最大贡献之一，是解释它为什么有效。

作者认为：

- `Intersection` 的本质作用是增强 **core locality**；
- 即让连续得到的 `UC` 更相似；
- 这样 SAT solver 在构造新 proof 时，可以更快复用局部结构。

论文进一步类比到 CDCL 里的 locality：

- 优先最近冲突相关变量，常常能更快找到证明；
- 在 CAR 里，优先最近 `UC` 的 literal，也会有类似作用。

### 4.4 作用方向

`Intersection` 更偏向：

- 让 `UNSAT proof` 更快；
- 让连续的 cores 更相似；
- 提升 proof 的收敛速度。

它主要优化的是：

- **proof locality**。

---

## 5. 旧方法二：Rotation

### 5.1 基本做法

`Rotation` 利用的信息不是 `last UC`，而是：

- 最近失败状态的公共部分。

论文里它维护一个 `common vector`，可以理解成：

- 最近若干个 failed states 的公共 literals。

然后做法是：

- 把当前 state `s` 中也出现在这个 `common vector` 里的 literals 提前。

即：

```text
priority = s ∩ commonFailedStates
```

### 5.2 直觉

`Rotation` 的直觉和 `Intersection` 不同。

它假设：

- 如果最近返回的 failed states 老是有相同的结构；
- 那说明搜索可能一直卡在同一片局部区域。

所以它希望：

- 优先这些公共 literals；
- 让新的 `UC` 更容易总结掉这片区域；
- 从而避免 solver 长时间困在某个局部子空间里。

### 5.3 作用方向

`Rotation` 更偏向：

- 减少反复探索相似失败状态；
- 降低找到 proof 所需 SAT 调用次数；
- 帮助搜索跳出坏的局部模式。

它主要优化的是：

- **search guidance**。

---

## 6. Intersection 和 Rotation 的关系

论文指出：

- `Intersection` 和 `Rotation` 都在做 assumption ordering；
- 但它们利用的是不同来源的历史信息。

可以概括成：

- `Intersection`：看最近一个 `UC`
- `Rotation`：看最近失败状态的公共结构

论文还指出：

- `Intersection` 里得到的 `iCube`，通常是 `Rotation` 里 `rCube` 的子集；
- 因为最近的 `UC` 本来就来自最近失败状态。

所以组合方式很自然：

- 先放 `Intersection` 产生的 literals；
- 再放 `Rotation` 产生但尚未出现的 literals；
- 最后再放其余 literals。

这就是文中的 `Combination(I+R)`。

---

## 7. 旧方法的局限

论文认为，`Intersection` 和 `Rotation` 虽然有效，但有一个共同问题：

- 它们都只决定了 assumptions 序列中“一部分 literal”的位置；
- 其余大量 literal 的顺序仍然没有被充分利用。

因此：

- 排序信号不够完整；
- 最近证明历史没有被系统地编码进 ordering 里。

这就是后面提出 `CoreLocality` 的动机。

---

## 8. 新方法一：CoreLocality

### 8.1 基本思想

`CoreLocality` 可以看成是对 `Intersection` 的推广。

如果说：

- `Intersection` 只看最近 1 个 `UC`

那么 `CoreLocality` 做的是：

- 看最近多个 `UC`；
- 按时间顺序把它们和当前 state 的交集依次放进排序结果里；
- 最后再补上 `Rotation` 的 `common vector`。

### 8.2 iLimit

论文引入参数：

```text
iLimit
```

表示：

- 最多使用多少个最近的 `UC`。

因此：

- `Local(2)` 表示使用最近 2 个 `UC`
- `Local(3)` 表示使用最近 3 个 `UC`
- 依此类推

### 8.3 排序流程

`CoreLocality` 的逻辑可以简化成：

1. 取最近第 1 个 `UC`，把和当前 state 相交的 literals 放进去；
2. 取最近第 2 个 `UC`，把尚未加入的相交 literals 放进去；
3. 一直到第 `iLimit` 个 `UC`；
4. 再把 `Rotation` 的 `common vector` 中尚未加入的 literals 放进去；
5. 最后接上剩余 literals。

所以它其实是：

- 多层次的 `Intersection`
- 再加一个 `Rotation` 尾部补充

### 8.4 为什么会有效

作者的直觉是：

- 只看一个最近 `UC` 太窄；
- 看最近多个 `UC`，能更准确描述当前 proof 的局部结构；
- 这样 assumptions 的顺序会更有信息量；
- 更多 literal 的顺序会被“有意义地决定”。

所以 `CoreLocality` 的作用是：

- 进一步增强 `core locality`；
- 更系统地利用最近 proof 历史；
- 给 solver 更稳定的排序引导。

### 8.5 iLimit 不是越大越好

论文特别强调：

- `iLimit` 的效果不是单调的；
- 增大 `iLimit` 并不保证性能更好。

原因是：

- `iLimit` 太小，信息不足；
- `iLimit` 太大，又会把太旧、可能过时的 `UC` 也纳入；
- 同时还会削弱 `Rotation` 的作用，因为前面的 `iCube` 占据了更多优先位置。

所以 `iLimit` 是一个 trade-off：

- 在“利用更多历史”与“保持局部性”之间平衡。

论文中的表格显示：

- `Local(4)` 在那组实验里最好；
- 但 `Local(5)` 又比 `Local(4)` 差一点；
- 这正说明它不是单调关系。

---

## 9. 新方法二：conflict-literal prioritization

### 9.1 conflict literal 是什么

论文指出：

- `UC` 里的 literal 并不都一样重要；
- 特别是“最后一个加入该 core 的 literal”，是这次 proof 所必需的；
- 作者把它叫做 `conflict literal`。

也就是说：

- 它往往是当前这个 `UC` 里最关键的成员之一。

### 9.2 prioritization 怎么做

做法非常简单：

- 如果某个 `iCube = (1, 2, 3)`；
- 且 `3` 是 conflict literal；
- 那么就把 `iCube` 重排为：

```text
(3, 1, 2)
```

也就是：

- 把 conflict literal 提到最前面；
- 其余保持相对顺序。

### 9.3 它的作用

作者的想法是：

- 既然 assumption 顺序会影响下一次 `UC`；
- 那么在最近 `UC` 里，最关键的 literal 更应该被优先处理；
- 这样更有利于 solver 复用相似的 proof 结构。

因此：

- `CoreLocality` 决定“参考哪些最近 `UC`”；
- `conflict-literal prioritization` 决定“同一个 `UC` 内谁最该排前面”。

论文也明确说：

- 从这一部分往后，当他们说 `CoreLocality` 时，默认就包含了这个优化。

---

## 10. 表格的阅读方法

论文里有一张表：

```text
Natural                0.94 / 0.95
Combination(I+R)       0.77 / 0.59
Local(2)               0.61 / 0.58
Local(3)               0.64 / 0.55
Local(4)               0.57 / 0.48
Local(5)               0.64 / 0.50
```

这里的 `Time (s)` 里两个数的含义是：

- 左值：`CoreLocality` 本身的平均 proof time
- 右值：加入 `conflict-literal prioritization` 后的平均 proof time

这个解释来自原文的两句话：

- 左值对应：`the left value of the 'Time' column`
- 右值对应：`the right value of the 'Time' column`

表的主要信息是：

1. `CoreLocality` 比 `Combination(I+R)` 更快
2. `iLimit` 的效果不是单调的
3. `conflict literal` 优先后会进一步提升性能

在这组实验里：

- `Local(4)` 是最优配置之一；
- 加上 conflict literal 后达到 `0.48s`。

---



## 11. 新方法三：Hybrid-CAR

### 11.1 动机

论文发现：

- `CoreLocality` 的最佳配置不是固定的；
- 不同 benchmark、不同阶段，最优 `iLimit` 可能不同；
- 同时 CAR 的 `U-sequence` 可能越来越大，导致某一轮搜索卡住很久。

所以作者提出：

- 不要把 ordering 配置固定死；
- 而要在运行时切换不同配置。

### 11.2 怎么做

`Hybrid-CAR` 的做法是：

- 给当前 ordering 配置一个时间上限；
- 如果超时还没有很好推进，就触发 restart；
- 切换到下一个 ordering 配置；
- 再继续搜索。

切换的主要对象是：

- 不同的 `CoreLocality(iLimit)` 配置。

### 11.3 restart 做什么

论文里的 restart 不是完全重来，而是：

- 保留 `U-sequence` 最低层的状态；
- 清掉更高层 `U`；
- 更换 ordering configuration；
- 重置计时器；
- 从头再跑。

这样做的目的是：

- 保留最基础的搜索信息；
- 但摆脱当前坏的局部搜索轨迹。

### 11.4 为什么可能有效

因为：

- 某个固定 ordering 可能在当前阶段效果很差；
- 持续使用它会让 CAR 长时间困在低效区域；
- 切换 ordering 相当于切换 proof-search 偏好；
- restart 则给新偏好一个真正重新组织搜索的机会。

### 11.5 论文中的定位

`Hybrid-CAR` 不是一个新的 ordering 公式，而是：

- 一个调度框架；
- 用来轮流尝试不同 ordering 配置。

因此它可以理解成：

- **portfolio over orderings**。

---

## 12. Par-2 score 的含义

论文还使用了 `Par-2 score` 来评价性能。

它的定义是：

- 对于在时间限制内完成的实例，记真实运行时间；
- 对于 timeout 的实例，不记无穷，而记为：

```text
2 × timeout_limit
```

然后再求平均。

所以：

- `Par-2` 越小越好；
- 它同时反映了解题速度和 timeout 惩罚；
- 比只看 solved count 更全面。

---

## 13. 论文的主要实验结论

从实验结果看，论文给出的主要结论包括：

1. `Intersection` 和 `Rotation` 都确实有效
   - 前者提升 `UNSAT proof` 的局部性；
   - 后者减少搜索反复卡在相似失败状态里的时间。

2. `CoreLocality` 比 `Combination(I+R)` 更强
   - 因为它更系统地利用了最近多个 `UC`；
   - 排序覆盖范围更大。

3. `conflict-literal prioritization` 能进一步加速
   - 在现有排序基础上进一步细化 `UC` 内部优先级。

4. `iLimit` 是一个重要但非单调的参数
   - 不是越大越好；
   - 最优值要靠实验或动态切换确定。

5. `Hybrid-CAR` 是最终最强的方案
   - 通过切换不同配置并 restart；
   - 显著优于固定配置；
   - 在论文实验中超过了原始 `SimpleCAR` 以及其他若干 bug-finding 基线。

---

## 14. 我对这篇论文的理解

我认为这篇论文最有价值的地方有三个。

第一，它把 assumption ordering 从“实现细节”提升成了一个明确的研究对象。

第二，它不只是提出新启发式，还解释了旧方法为什么有效：

- `Intersection` 的本质作用是 `core locality`；
- 也就是让连续 proof 在局部结构上更相似。

第三，它给出了一个很自然的演进路线：

```text
Intersection
→ Rotation
→ Combination(I+R)
→ CoreLocality
→ conflict-literal prioritization
→ Hybrid-CAR
```

这条线非常清楚地体现出：

- 从利用最近一个 `UC`
- 到利用多个 `UC`
- 再到动态切换多组 ordering

整体思路始终一致：

- **更好地组织 assumption literal 的顺序，来诱导更有利的 UC 和 proof。**

---

## 15. 一句话总结

这篇论文的核心结论可以概括成：

- 在 CAR 中，assumption literal ordering 会实质性改变 `UC` 和 proof 的形状；
- `Intersection`、`Rotation`、`CoreLocality`、`conflict-literal prioritization` 和 `Hybrid-CAR` 都是在沿着这条链条做优化；
- 其中 `Hybrid-CAR` 是把多种 ordering 配置作为动态策略来调度，从而取得最好的 bug-finding 效果。
