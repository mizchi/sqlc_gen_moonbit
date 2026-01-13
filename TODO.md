# TODO - sqlc-gen-moonbit

sqlc標準機能との比較に基づく実装ロードマップ

## 現在の実装状況

### サポート済み
- [x] `:one` - 単一行取得 (`T?`)
- [x] `:many` - 複数行取得 (`Array[T]`)
- [x] `:exec` - 実行のみ (`Unit`)
- [x] `:execrows` - 影響行数を返す (`Int`)
- [x] `:execlastid` - 最終挿入IDを返す (`Int64`)
- [x] SQLiteバックエンド (native)
- [x] Cloudflare D1バックエンド
- [x] 基本型マッピング (INTEGER, TEXT, REAL, BLOB, BOOLEAN)
- [x] NULL対応 (Option型)
- [x] バリデーター生成 (オプション)
- [x] JSON Schema生成 (オプション)
- [x] WASM配布
- [x] Cursor-based pagination
- [x] インデックス最適化

---

## ディレクトリ構造

```
examples/           # シンプルな使用例
  sqlite-basic/     # SQLite native 最小構成
  d1-basic/         # Cloudflare D1 最小構成
tests/              # 包括的なテストフィクスチャ
  sqlite-native/    # SQLite native 全機能テスト (38 tests)
  d1/               # D1 全機能テスト (44 tests)
```

---

## ~~Priority 0: 本番運用向けクエリチューニング~~ ✅ 実装完了

### ~~Phase 1: インデックス追加~~ ✅ 実装完了

`tests/sqlite-native/db/sqlite/schema.sql` にインデックス追加済み:
- `idx_posts_author_id`, `idx_posts_created_at`, `idx_posts_is_published`
- `idx_users_is_active`, `idx_users_name`

### ~~Phase 2: ページネーション改善~~ ✅ 実装完了

Cursor-based pagination クエリ追加済み:
- `ListUsersPaginated`, `ListPostsByAuthorPaginated`
- `ListPostsWithAuthorsPaginated`, `ListPublishedPostsPaginated`

### Phase 3: 追加クエリパターン（将来）

優先度低めだが検討：

```sql
-- UPSERT (存在すれば更新、なければ作成)
-- name: UpsertUser :exec
INSERT INTO users (email, name) VALUES (?, ?)
ON CONFLICT(email) DO UPDATE SET name = excluded.name;

-- IN リストクエリ (複数ID一括取得)
-- 注意: sqlc は動的な IN リストを直接サポートしない
-- name: GetUsersByIds :many
SELECT * FROM users WHERE id IN (/*SLICE:ids*/?)

-- EXISTS による効率的な存在チェック
-- name: UserExistsByEmail :one
SELECT EXISTS(SELECT 1 FROM users WHERE email = ?) as exists;
```

---

## Priority 1: 実用上必須

### ~~`:execrows` / `:execlastid` サポート~~ ✅ 実装完了

影響行数やINSERT後のIDを取得する機能。

```sql
-- name: DeleteOldPosts :execrows
DELETE FROM posts WHERE created_at < ?;

-- name: CreateUser :execlastid
INSERT INTO users (name, email) VALUES (?, ?);
```

- SQLite: `sqlite_changes()` / `sqlite_last_insert_rowid()` 使用
- D1: `stmt.run()` の `meta.changes` / `meta.last_row_id` を返す

### JOINクエリの型推論改善
現状JOINクエリは動作するが、カラム名の曖昧さ解決が不十分。

```sql
-- name: GetPostWithAuthor :one
SELECT p.*, u.name as author_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.id = ?;
```

**課題**: 同名カラムの衝突、エイリアス処理

### トランザクションサポート
複数クエリをアトミックに実行するためのヘルパー。

**実装方針**:
- SQLite: `BEGIN`/`COMMIT`/`ROLLBACK` ラッパー関数生成
- D1: `db.batch()` 使用

---

## Priority 2: 開発体験向上

### `:execresult` サポート
実行結果のメタデータ（影響行数、最終挿入ID）を構造体で返す。

```moonbit
pub struct ExecResult {
  rows_affected : Int64
  last_insert_id : Int64
}
```

### エラーハンドリング改善
現状エラーは無視されるか、単にNoneを返す。Result型での明示的エラー伝播。

```moonbit
// 現状
pub fn get_user(db, params) -> GetUserRow?

// 改善案
pub fn get_user(db, params) -> Result[GetUserRow?, DbError]
```

**検討事項**:
- SQLiteエラーコードのマッピング
- D1のJavaScriptエラーの処理

### 日時型サポート
SQLiteのDATETIME/TIMESTAMP型を専用型にマッピング。

**選択肢**:
1. `String` のまま（現状）- ユーザー側でパース
2. `@time.Time` や独自型にマッピング
3. Unix timestamp (`Int64`) として扱う

---

## Priority 3: 高度な機能

### `:copyfrom` サポート
大量データの一括挿入。SQLiteでは`INSERT ... VALUES (...), (...), ...`にコンパイル。

```sql
-- name: BulkInsertUsers :copyfrom
INSERT INTO users (name, email) VALUES (?, ?);
```

### バッチ操作 (`:batchexec`, `:batchmany`, `:batchone`)
PostgreSQL pgx由来の機能。SQLite/D1での代替実装を検討。

**D1での実装**: `db.batch([stmt1, stmt2, ...])` を使用

### ENUM型サポート
SQLiteにはネイティブENUMがないが、CHECK制約からの推論が可能。

```sql
CREATE TABLE posts (
  status TEXT CHECK(status IN ('draft', 'published', 'archived'))
);
```

```moonbit
pub enum PostStatus {
  Draft
  Published
  Archived
}
```

### 配列型サポート
PostgreSQL配列やJSON配列のマッピング。

---

## Priority 4: エコシステム拡張

### PostgreSQLバックエンド
新規バックエンド追加。MoonBitのPostgreSQLバインディングが必要。

### MySQLバックエンド
新規バックエンド追加。MoonBitのMySQLバインディングが必要。

### カスタム型オーバーライド
sqlc.yamlでの型マッピングカスタマイズ。

```yaml
overrides:
  - column: "users.id"
    go_type: "int64"  # -> moonbit_type: "Int64"
```

### Prepared Statement キャッシュ
クエリごとにprepareするのではなく、初回prepare結果を再利用。

---

## 技術的負債

### コード生成のリファクタリング
`lib/codegen/codegen.mbt` が1200行超で肥大化。機能別に分割:
- `types.mbt` - 型定義生成
- `queries.mbt` - クエリ関数生成
- `validators.mbt` - バリデーター生成

### テストカバレッジ拡充
- NULL値を含むクエリのテスト
- エッジケース（空テーブル、大量データ）
- 不正なSQLの検出テスト

### ドキュメント整備
- 型マッピング表の詳細化
- バックエンド固有の制限事項
- マイグレーションガイド

---

## 参考: sqlc標準クエリアノテーション

| アノテーション | 説明 | sqlc-gen-moonbit |
|--------------|------|-----------------|
| `:one` | 単一行取得 | ✅ 実装済み |
| `:many` | 複数行取得 | ✅ 実装済み |
| `:exec` | 実行のみ | ✅ 実装済み |
| `:execrows` | 影響行数を返す | ✅ 実装済み |
| `:execlastid` | 最終挿入IDを返す | ✅ 実装済み |
| `:execresult` | 結果メタデータを返す | 未実装 |
| `:copyfrom` | 一括挿入 | 未実装 |
| `:batchexec` | バッチ実行 | 未実装 |
| `:batchmany` | バッチ複数行取得 | 未実装 |
| `:batchone` | バッチ単一行取得 | 未実装 |
