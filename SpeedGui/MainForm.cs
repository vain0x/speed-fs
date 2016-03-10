using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace SpeedGui
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();
        }

        private void DrawTitle(Graphics g)
        {
            g.DrawString
                ("Speed F#"
                , new Font("Yu Gothic", 35)
                , new SolidBrush(Color.Black)
                , new PointF(30, 50)
                );

            g.DrawString
                ("Click to Start!"
                , new Font("Yu Gothic", 16)
                , new SolidBrush(Color.Black)
                , new PointF(70, 150)
                );
        }

        private void MainForm_Paint(object sender, PaintEventArgs e)
        {
            DrawTitle(e.Graphics);
        }
    }
}
