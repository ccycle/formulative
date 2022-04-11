# Test

- Testの作り方
  - test用のモジュールを`test-tasty/Test/`以下に作成
    - testしたい関数を含むmoduleをimportする
      - testしたい関数を書く
        - rule:
        - unit test
          - 接頭辞に`unit_`をつける
            - 例: `unit_func = ...`
        - quickcheck
          - 接頭辞に`prop_`をつける
            - 例: `prop_func = ...`
    - testの実行: `stack test formulative:test:formulative-tasty`

## 参考文献

- [テストフレームワーク (tasty)](https://haskell.e-bigmoon.com/stack/test/tasty.html)
