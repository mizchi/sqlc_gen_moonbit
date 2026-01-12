# D1 Example

Cloudflare D1 を使った sqlc-gen-moonbit の例。

## 前提条件

- [sqlc](https://sqlc.dev/)
- [atlas](https://atlasgo.io/)
- [wrangler](https://developers.cloudflare.com/workers/wrangler/)
- [moon](https://www.moonbitlang.com/)

## ディレクトリ構成

```
examples/d1/
├── atlas.hcl           # Atlas 設定
├── db/
│   ├── schema.sql      # スキーマ定義 (Atlas 管理)
│   ├── sqlite/
│   │   └── query.sql   # SQL クエリ定義
│   ├── migrations/     # マイグレーションファイル (Atlas 生成)
│   └── gen/            # 生成コード (sqlc 生成)
├── app/                # Worker アプリケーション
├── test/               # テスト
├── sqlc.yaml           # sqlc 設定
├── wrangler.toml       # Wrangler 設定
└── worker.js           # Worker エントリポイント
```

## ワークフロー

### 1. スキーマを編集

`db/schema.sql` を編集:

```sql
-- 新しいカラムを追加
ALTER TABLE users ADD COLUMN phone TEXT;
```

### 2. マイグレーション生成

```bash
just migrate-diff add_phone_column
```

### 3. コード再生成

```bash
just generate-d1
```

### 4. ローカル D1 にマイグレーション適用

```bash
just migrate-apply
```

### 5. テスト実行

```bash
just test-d1
```

### 6. 開発サーバー起動

```bash
just dev-d1
```

## コマンド一覧

| コマンド | 説明 |
|---------|------|
| `just generate-d1` | sqlc でコード生成 |
| `just test-d1` | テスト実行 |
| `just build-d1` | Worker ビルド |
| `just dev-d1` | 開発サーバー起動 |
| `just migrate-diff <name>` | schema.sql の差分からマイグレーション生成 |
| `just migrate-apply` | ローカル D1 にマイグレーション適用 |
| `just migrate-apply-remote` | リモート D1 にマイグレーション適用 |
| `just migrate-status` | マイグレーション状態を表示 |
| `just migrate-lint` | マイグレーションを検証 |
| `just migrate-hash` | マイグレーションのハッシュを更新 |

## 生成されるファイル

- `db/gen/sqlc_types.mbt` - 型定義 (Row, Params)
- `db/gen/sqlc_queries.mbt` - クエリ関数
- `db/gen/sqlc_validators.mbt` - バリデーション関数
- `db/gen/sqlc_schema.json` - JSON Schema
- `db/gen/moon.pkg` - パッケージ設定

## API エンドポイント (Worker)

```
GET  /api/users          - ユーザー一覧
GET  /api/users/:id      - ユーザー取得
GET  /api/posts          - 投稿一覧
GET  /api/categories     - カテゴリ一覧
GET  /api/tags           - タグ一覧
```
