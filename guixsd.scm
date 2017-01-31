(use-modules (guix build-system trivial)
             (guix download)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (gnu)
             (gnu system nss))
(use-service-modules avahi base cups dbus desktop networking sddm xorg)
(use-package-modules admin certs cups curl disk dns emacs fonts fontutils gnome
                     gnupg linux mail password-utils shells ssh terminals
                     version-control)

(define-public linux-firmware
  (let ((commit "6d3bc8886517d171068fd1263176b8b5c51df204"))
    (package
      (name "linux-firmware")
      (version (string-append "2017.01.26-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                        (url (string-append
                              "https://github.com/wkennington/linux-firmware"))
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

           (let* ((source   (assoc-ref %build-inputs "source"))
                  (out      (assoc-ref %outputs "out"))
                  (firmware (string-append out "/lib/firmware")))
             (mkdir-p firmware)
             (copy-recursively source firmware)))))
      (home-page "https://github.com/wkennington/linux-firmware")
      (synopsis "Linux firmware")
      (description "Linux firmware.")
      (license #f))))

(define-public linux
  (package
    (inherit linux-libre)
    (version "4.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cdn.kernel.org/pub/linux/kernel/v4.x/"
                    "linux-" version ".tar.xz"))
              (sha256
               (base32
                "16yfrydxcdlbm8vmfqirc0gshsbka6mjgfwc2wqs422v19vsz4zl"))))))

(operating-system
  (host-name "xps13")
  (timezone "Europe/Zurich")
  (locale "en_US.UTF-8")

  (bootloader (grub-configuration (device "/dev/nvme0n1")))
  (kernel linux)
  (firmware (list linux-firmware))

  (file-systems (cons* (file-system
                         (device "/dev/nvme0n1p2")
                         ;; FIXME: FAT32 labels are not supported.
                         ;;(title 'label)
                         (mount-point "/boot/efi")
                         (type "vfat")
                         ;; FIXME: Required so base-initrd gets updated.
                         (needed-for-boot? #t))
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

  (packages (cons* gnome-themes-standard ;gnome-disk-utility
                   emacs font-adobe-source-code-pro
                   gptfdisk tree which
                   nss-certs bind curl isc-dhcp gptfdisk wpa-supplicant
                   gnupg pinentry openssh picocom mutt
                   git (git "send-email") ; git-crypt git-annex
                   fontconfig font-dejavu font-ubuntu font-gnu-unifont
                   password-store hplip
                   %base-packages))

  (services (cons* ;; Network.
                   ;;(service network-manager-service-type
                   ;; (network-manager-configuration))
                   (dhcp-client-service)
                   ;;(service wpa-supplicant-service-type wpa-supplicant)

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
                   (xfce-desktop-service)

                   ;; Misc.
                   (bluetooth-service)
                   (service cups-service-type
                    (cups-configuration
                     (web-interface? #t)))

                   ;; Jobs.
                   ;; TODO: Try mcron and rotlog.

                   ;; Other.
                   (rngd-service)
                   %base-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
