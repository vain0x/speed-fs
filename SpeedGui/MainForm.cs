using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.FSharp;
using Microsoft.FSharp.Core;

using Speed;
using Spc = Speed.Core;
using Spct = Speed.Core.Types;

namespace SpeedGui
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();
        }

        Spct.GameT<Spct.PlayerT<Unit, Unit>> gs_;

        private void DrawTitle(Graphics gx)
        {
            gx.DrawString
                ("Speed F#"
                , new Font("Yu Gothic", 35)
                , new SolidBrush(Color.Black)
                , new PointF(30, 50)
                );

            gx.DrawString
                ("Click to Start!"
                , new Font("Yu Gothic", 16)
                , new SolidBrush(Color.Black)
                , new PointF(70, 150)
                );
        }

        private void DrawBoard(Graphics gx)
        {
        }

        private void StartGame()
        {
            // unimplemented
        }

        private void MainForm_Paint(object sender, PaintEventArgs e)
        {
            DrawTitle(e.Graphics);
        }

        private void MainForm_Click(object sender, EventArgs e)
        {
            StartGame();
        }
    }
}
