(use-modules (gnu) (gnu system linux-initrd) (gnu system nss)
             (guix download) (guix git-download)
             (guix gexp) (guix modules) (guix monads) (guix packages)
             (guix store) (guix utils)
             (guix build-system gnu)
             (guix build-system trivial)
             (ice-9 match))
(use-service-modules avahi base cups dbus desktop networking sddm xorg)
(use-package-modules admin certs cpio cups curl disk dns emacs firmware fonts
                     fontutils gnome gnupg linux mail password-utils patchutils
                     shells ssh terminals version-control)

(define-public linux-firmware
  (let ((commit "6d3bc8886517d171068fd1263176b8b5c51df204"))
    (package
      (name "linux-firmware")
      (version (string-append "2017.01.26-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wkennington/linux-firmware")
                      (commit commit)))
                (sha256
                 (base32
                  "15qm9fzv8rjhzyrqjdd4dqd6slymiz0w6wn7likbfjvh2szczafs"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((src (assoc-ref %build-inputs "source"))
                  (out (assoc-ref %outputs "out"))
                  (fmw (string-append out "/lib/firmware")))
             (mkdir-p fmw)
             (copy-recursively src fmw)))))
      (home-page "https://github.com/wkennington/linux-firmware")
      (synopsis "Linux firmware")
      (description "Linux firmware.")
      (license #f))))

(define-public microcode
  (package
    (name "microcode")
    (version "20161104")
    (source #f)
    (build-system gnu-build-system)
    (native-inputs
     `(;;("cpio" ,cpio)
       ("microcode.dat"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://downloadmirror.intel.com/26400/eng/"
                               "microcode-" version ".tgz"))
           (sha256
            (base32
             "1lg3bvznvwcxf66k038c57brkcxfva8crpnzj5idmczr5yk4q5bh"))))
       ("microcode2ucode"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/NixOS/nixpkgs/"
                               "56904d7c423f2b13b37fbd29f39bbb4b52bc7824"
                               "/pkgs/os-specific/linux/microcode/"
                               "intel-microcode2ucode.c"))
           (sha256
            (base32
             "1ph3zq76dkikvxyrbpaxx8d9302bzl3n1d71qzcx790ncfb3m883"))))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'unpack)
         (delete 'configure)
         (delete 'check)
         (delete 'install)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mc  (assoc-ref inputs "microcode.dat"))
                    (m2u (assoc-ref inputs "microcode2ucode"))
                    (fmw (string-append out "/lib/firmware/microcode")))
               (system* "tar" "-xf" mc)
               (system* "gcc" "-Wall" "-o" "m2u" m2u)
               (system* "./m2u" "microcode.dat")
               (mkdir-p fmw)
               (copy-file "microcode.bin" (string-append fmw "/GenuineIntel.bin"))))))))
    (home-page "http://www.intel.com")
    (synopsis "Microcode for Intel processors")
    (description "Microcode for Intel processors.")
    (license #f)))

(define-public linux
  (package
    (inherit linux-libre)
    (version "4.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cdn.kernel.org/pub/linux/kernel/v4.x/"
                                  "linux-" version ".tar.xz"))
              (sha256
               (base32
                "16yfrydxcdlbm8vmfqirc0gshsbka6mjgfwc2wqs422v19vsz4zl"))))
    (native-inputs
     `(("kconfig" ,(local-file "/home/dvc/kernel-config"))
       ,@(filter (match-lambda
                   (("kconfig" _) #f)
                   (_ #t))
                 (package-native-inputs linux-libre))))
    (inputs
     `(("microcode" ,microcode)
       ,@(package-inputs linux-libre)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux-libre)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-microcode-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((microcode (assoc-ref inputs "microcode")))
                 (substitute* "arch/x86/kernel/cpu/microcode/intel.c"
                   (("kernel/x86/microcode/GenuineIntel.bin")
                    (string-append "/lib/firmware/microcode/GenuineIntel.bin"))))
               #t))))))))

(define* (initrd file-systems #:key #:allow-other-keys)

  (define initrd-linux-modules
    ;; Modules added to the initrd and loaded from the initrd.
    (list "btrfs"
          "i2c-hid"))

  (define initrd-packages
    ;; Packages to be copied on the initrd.
    (list btrfs-progs/static
          microcode))

  (define flat-linux-module-directory
    (@@ (gnu system linux-initrd) flat-linux-module-directory))

  (mlet %store-monad ((kodir (flat-linux-module-directory linux
                                                          initrd-linux-modules)))
    (expression->initrd
     (with-imported-modules (source-module-closure
                             '((gnu build linux-boot)
                               (guix build utils)
                               (guix build bournish)
                               (gnu build file-systems)))
       #~(begin
           (use-modules (gnu build linux-boot)
                        (guix build utils)
                        (guix build bournish) ;add the 'bournish' meta-command
                        (srfi srfi-26))

           (with-output-to-port (%make-void-port "w")
             (lambda ()
               (set-path-environment-variable "PATH" '("bin" "sbin")
                                              '#$initrd-packages)))

           (boot-system #:mounts '#$(map file-system->spec file-systems)
                        #:linux-modules '#$initrd-linux-modules
                        #:linux-module-directory '#$kodir)))
     #:name "base-initrd")))

(operating-system
  (host-name "xps13")
  (timezone "Europe/Zurich")
  (locale "en_US.UTF-8")

  (bootloader (grub-configuration (device "/dev/nvme0n1")))
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd initrd)

  (file-systems (cons* #!(file-system
                         (device "/dev/nvme0n1p2")
                         ;; FIXME: FAT32 labels are not supported.
                         ;;(title 'label)
                         (mount-point "/boot/efi")
                         (type "vfat")
                         ;; FIXME: Required so base-initrd gets updated.
                         (needed-for-boot? #t))!#
                       (file-system
                         (device "root")
                         (title 'label)
                         (mount-point "/")
                         (type "btrfs"))
                       %base-file-systems))

  (swap-devices '("/dev/nvme0n1p3"))

  (users (cons (user-account
                (name "dvc")
                (group "users")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "lp" "lpadmin"))
                (home-directory "/home/dvc")
                (shell #~(string-append #$zsh "/bin/zsh")))
               %base-user-accounts))

  (packages (cons* gnome-themes-standard gnome-calculator gnome-disk-utility
                   emacs font-adobe-source-code-pro
                   colordiff gptfdisk tree which
                   nss-certs bind curl gptfdisk
                   gnupg pinentry openssh picocom mutt
                   git `(,git "send-email") git-crypt ; git-annex
                   fontconfig font-dejavu font-ubuntu font-gnu-unifont
                   password-store hplip
                   %base-packages))

  (services (cons* ;; Network.
                   (service network-manager-service-type
                    (network-manager-configuration))
                   (service wpa-supplicant-service-type wpa-supplicant)

                   ;; Selected services from %desktop-services.
                   (avahi-service)
                   (colord-service)
                   (dbus-service)
                   (elogind-service)
                   (geoclue-service)
                   (polkit-service)
                   (udisks-service)
                   (upower-service)
                   (ntp-service)

                   ;; Display manager and desktop.
                   (sddm-service)
                   (gnome-desktop-service)

                   (bluetooth-service)
                   (service cups-service-type
                    (cups-configuration
                     (web-interface? #t)))

                   ;; Jobs.
                   ;; TODO: Try mcron and rotlog.

                   (rngd-service)
                   %base-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
