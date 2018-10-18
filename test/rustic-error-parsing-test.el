;; -*- lexical-binding: t -*-

(defun rustic-test-count-error-helper (string)
  (let* ((buffer (get-buffer-create "b"))
         (dir (rustic-babel-generate-project t))
         (src (concat dir "/src"))
         (file (expand-file-name "main.rs" src))
         (buffer-save-without-query t)
         (rustic-format-on-save nil))
    (with-current-buffer buffer
      (write-file file)
      (insert string)
      (save-buffer))
    dir))

(ert-deftest rustic-test-backtrace ()
  (let* ((string "fn main() {
                       let v = vec![1, 2, 3];
                       v[99];
                    }")
         (default-directory (rustic-test-count-error-helper string)))
    (let ((rustic-compile-backtrace "0")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))
    (let ((rustic-compile-backtrace "1")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))
    (let ((rustic-compile-backtrace "full")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))))

(ert-deftest rustic-test-count-errors ()
  ;; test error without error code
  (let* ((string "fn main() {")
         (default-directory (rustic-test-count-error-helper string)))
    (let ((proc (rustic-compilation-start (split-string "cargo build"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))))

(ert-deftest rustic-test-count-warnings ()
  (let* ((string "fn main() {
                       let v1 = vec![1, 2, 3];
                       let v2 = vec![1, 2, 3];
                    }")
         (default-directory (rustic-test-count-error-helper string))
         (proc (rustic-compilation-start (split-string "cargo build"))))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (with-current-buffer (get-buffer rustic-compilation-buffer-name)
      (should (= compilation-num-warnings-found 2)))))

(ert-deftest rustic-test-cargo-test ()
  ;; compilation-num-errors-found would be 8 with regular compilaton mode
  ;; due to parsing issues https://github.com/rust-lang/rust-mode/pull/254
  (let ((rustic-compile-backtrace "full"))
    (let* ((string "#[cfg(test)]
                  mod tests {
                      #[test]
                      fn it_works() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works2() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works3() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works4() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works5() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works6() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works7() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works8() {
                          assert_eq!(2 + 2, 3);
                      }

                      // compile.el stops parsing here

                      #[test]
                      fn it_works9() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works10() {
                          assert_eq!(2 + 2, 3);
                      }
                  }")
           (default-directory (rustic-test-count-error-helper string))
           (proc (rustic-cargo-test)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-test-buffer-name)
        (should (= compilation-num-errors-found 10))))))

(ert-deftest rustic-test-info-count ()
  (let* ((buffer1 (get-buffer-create "test-info1"))
         (buffer2 (get-buffer-create "test-info2"))
         (string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t)))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (file2 (progn (shell-command-to-string "touch src/test.rs")
                         (expand-file-name "test.rs" src))))


      )
    ))
