# ====== 初始化检查 ======
Write-Host "加载系统优化配置..." -ForegroundColor Cyan

# 工具检查函数
function Test-ToolInstalled {
    param([string]$Command, [string]$InstallHint)
    if (-not (Get-Command $Command -ErrorAction SilentlyContinue)) {
        Write-Host "  [!] 缺失工具: $InstallHint" -ForegroundColor Yellow
        return $false
    }
    return $true
}

# 批量检查工具
$tools = @(
    @{Command = "fd"; InstallHint = "fd (winget install sharkdp.fd)" },
    @{Command = "eza"; InstallHint = "eza (winget install eza-community.eza)" },
    @{Command = "bat"; InstallHint = "bat (winget install sharkdp.bat)" },
    @{Command = "fzf"; InstallHint = "fzf (winget install junegunn.fzf)" }
)

$missingCount = 0
foreach ($tool in $tools) {
    if (-not (Test-ToolInstalled $tool.Command $tool.InstallHint)) {
        $missingCount++
    }
}

if ($missingCount -gt 0) {
    Write-Warning "$missingCount 个工具未安装，部分功能受限"
}

# ====== 命令历史增强 ======
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# ====== 智能别名系统 ======
function Invoke-EzaEnhanced {
    param(
        [switch]$All,
        [switch]$Tree,
        [int]$Level = 2,
        [Parameter(ValueFromRemainingArguments = $true)]
        $ExtraArgs
    )
    
    $params = @(
        "--icons=always",
        "--git",
        "--color=auto",
        "--group-directories-first"
    )
    
    if ($All) { $params += "-al" }
    if ($Tree) { $params += "--tree"; $params += "--level=$Level" }
    if ($ExtraArgs) { $params += $ExtraArgs }
    
    eza @params
}

# 修复：使用函数代替别名
function ls { Invoke-EzaEnhanced @args }
function la { Invoke-EzaEnhanced -All @args }
function lt { Invoke-EzaEnhanced -Tree @args }

# ====== 安全操作函数 ======
function Invoke-SafeOperation {
    param(
        [Parameter(Mandatory = $true)]
        [ValidateSet('Copy', 'Move', 'Remove')]
        [string]$Operation,
        [Parameter(ValueFromRemainingArguments = $true)]
        [string[]]$Paths
    )
    
    if (-not $Paths) {
        Write-Warning "未指定操作路径"
        return
    }
    
    $cmd = switch ($Operation) {
        'Copy' { Get-Command Copy-Item }
        'Move' { Get-Command Move-Item }
        'Remove' { Get-Command Remove-Item }
    }
    
    $confirmed = $true
    if ($Paths.Count -gt 3) {
        $choice = Read-Host "确认要对 $($Paths.Count) 个项目执行 $Operation 操作? (Y/N)"
        $confirmed = ($choice -eq 'Y')
    }
    
    if ($confirmed) {
        try {
            & $cmd @Paths -Confirm:$($Paths.Count -le 3) -ErrorAction Stop
        }
        catch {
            Write-Warning "操作失败: $_"
        }
    }
    else {
        Write-Host "操作已取消" -ForegroundColor Yellow
    }
}

# 修复：使用函数代替别名
function cp { Invoke-SafeOperation Copy @args }
function mv { Invoke-SafeOperation Move @args }
function rm { Invoke-SafeOperation Remove @args }

# ====== 系统监控 ======
function Get-DiskUsage {
    Get-Volume | Where-Object DriveLetter | ForEach-Object {
        [PSCustomObject]@{
            Drive       = $_.DriveLetter
            Filesystem  = $_.FileSystemLabel
            SizeGB      = [math]::Round($_.Size / 1GB, 1)
            FreeGB      = [math]::Round($_.SizeRemaining / 1GB, 1)
            FreePercent = [math]::Round(($_.SizeRemaining / $_.Size) * 100, 1)
        }
    } | Format-Table -AutoSize
}

function Get-MemoryUsage {
    $os = Get-CimInstance Win32_OperatingSystem
    $total = $os.TotalVisibleMemorySize / 1MB
    $free = $os.FreePhysicalMemory / 1MB
    
    [PSCustomObject]@{
        TotalGB = [math]::Round($total / 1KB, 2)
        UsedGB  = [math]::Round(($total - $free) / 1KB, 2)
        FreeGB  = [math]::Round($free / 1KB, 2)
        Usage   = [math]::Round((($total - $free) / $total) * 100, 2)
    } | Format-Table -AutoSize
}

Set-Alias -Name df -Value Get-DiskUsage
Set-Alias -Name free -Value Get-MemoryUsage

# ====== 进程监控 ======
function Get-TopProcesses {
    param(
        [ValidateSet('CPU', 'Memory')]
        [string]$SortBy = 'Memory',
        [int]$Top = 5
    )
    
    $property = switch ($SortBy) {
        'CPU' { 'CPU' }
        'Memory' { 'WorkingSet' }
    }
    
    Get-Process | 
    Sort-Object $property -Descending | 
    Select-Object -First $Top ProcessName, Id,
    @{Name = "CPU(s)"; Expression = { [math]::Round($_.CPU, 2) } },
    @{Name = "Memory(MB)"; Expression = { [math]::Round($_.WorkingSet / 1MB, 2) } }
}

# 修复：使用函数代替别名（解决脚本块问题）
function pscpu { Get-TopProcesses -SortBy CPU @args }
function psmem { Get-TopProcesses -SortBy Memory @args }

# ====== 工具集成 ======
# Zoxide 智能目录跳转
if (Test-ToolInstalled "zoxide" "zoxide (winget install ajeetdsouza.zoxide)") {
    Invoke-Expression (& { (zoxide init powershell | Out-String) })
    Set-Alias -Name cd -Value z -Option AllScope
}

# Neovim 集成
if (Test-ToolInstalled "nvim" "Neovim (winget install Neovim.Neovim)") {
    Set-Alias -Name vim -Value nvim -Option AllScope
    Set-Alias -Name vi -Value nvim -Option AllScope
}

# ====== 终端美化 ======
# Oh My Posh 主题
if (Test-ToolInstalled "oh-my-posh" "Oh My Posh (winget install JanDeDobbeleer.OhMyPosh)") {
    # 动态查找主题路径
    $themePaths = @(
        "${env:POSH_THEMES_PATH}\tokyo.omp.json",
        "${env:USERPROFILE}\AppData\Local\Programs\oh-my-posh\themes\tokyo.omp.json",
        "${env:ProgramFiles}\oh-my-posh\themes\tokyo.omp.json"
    )
    
    $themePath = $themePaths | Where-Object { Test-Path $_ } | Select-Object -First 1
    
    if ($themePath) {
        oh-my-posh init pwsh --config $themePath | Invoke-Expression
    }
    else {
        oh-my-posh init pwsh | Invoke-Expression
        Write-Warning "使用默认主题: tokyo 主题未找到"
    }
}

# Terminal Icons
if (-not (Get-Module -Name Terminal-Icons -ErrorAction SilentlyContinue)) {
    try {
        Import-Module Terminal-Icons -ErrorAction Stop
    }
    catch {
        try {
            Install-Module Terminal-Icons -Scope CurrentUser -Force -ErrorAction Stop
            Import-Module Terminal-Icons
        }
        catch {
            Write-Warning "Terminal-Icons 加载失败: $_"
        }
    }
}

# ====== 文件快速导航 ======
if (Get-Module -ListAvailable -Name PSFzf) {
    try {
        Import-Module PSFzf -ErrorAction Stop
        Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'
        
        # 智能预览配置
        $previewCommand = if (Get-Command bat -ErrorAction SilentlyContinue) {
            "bat --style=numbers --color=always --line-range :300 {1}"
        }
        else {
            "Get-Content {1} -Head 30"
        }
        
        $env:FZF_DEFAULT_OPTS = @"
--height 40% 
--border
--preview '$previewCommand'
--preview-window 'right:60%'
"@
    }
    catch {
        Write-Warning "PSFzf 模块加载失败: $_"
    }
}

Write-Host "系统优化配置加载完成!" -ForegroundColor Green